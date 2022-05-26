package ee.hitsa.ois.service.ehis;

import java.lang.invoke.MethodHandles;
import java.math.BigInteger;
import java.time.LocalDate;
import java.util.List;
import java.util.function.Consumer;

import javax.annotation.PostConstruct;
import javax.persistence.EntityManager;
import javax.xml.datatype.DatatypeConfigurationException;
import javax.xml.datatype.DatatypeFactory;
import javax.xml.datatype.XMLGregorianCalendar;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;

import ee.hitsa.ois.domain.Classifier;
import ee.hitsa.ois.domain.Person;
import ee.hitsa.ois.domain.WsEhisStudentLog;
import ee.hitsa.ois.domain.curriculum.Curriculum;
import ee.hitsa.ois.domain.directive.Directive;
import ee.hitsa.ois.domain.student.Student;
import ee.hitsa.ois.enums.Language;
import ee.hitsa.ois.exception.BadConfigurationException;
import ee.hitsa.ois.exception.HoisException;
import ee.hitsa.ois.util.EntityUtil;
import ee.hitsa.ois.util.ExceptionUtil;
import ee.hitsa.ois.util.StreamUtil;
import ee.hitsa.ois.util.StudentUtil;
import ee.hitsa.ois.util.Translatable;
import ee.hitsa.ois.util.TranslateUtil;
import ee.hitsa.ois.validation.ValidationFailedException;
import ee.hois.soap.LogContext;
import ee.hois.xroad.ehis.generated.KhlDuplikaadiMuutmine;
import ee.hois.xroad.ehis.generated.KhlIsikuandmedLisa;
import ee.hois.xroad.ehis.generated.KhlLisamine;
import ee.hois.xroad.ehis.generated.KhlMuutmine;
import ee.hois.xroad.ehis.generated.KhlOppeasutus;
import ee.hois.xroad.ehis.generated.KhlOppeasutusList;
import ee.hois.xroad.ehis.generated.KhlOppur;
import ee.hois.xroad.ehis.service.EhisClient;
import ee.hois.xroad.ehis.service.EhisLaeKorgharidusedResponse;
import ee.hois.xroad.helpers.XRoadHeaderV4;

public abstract class EhisService {

    private static final Logger log = LoggerFactory.getLogger(MethodHandles.lookup().lookupClass());

    private static final String ERROR_MARKER_RESULT = "Viga";
    private static final String ERROR_MARKER_MSG = "VIGA";
    static final String LAE_KORGHARIDUS_SERVICE_CODE = "laeKorgharidus";
    public static final String LAE_KORGHARIDUS_SERVICE = "ehis."+ LAE_KORGHARIDUS_SERVICE_CODE;
    private static final String BIRTH_DATE_ENTERED = "SS";

    private DatatypeFactory datatypeFactory;
    @Autowired
    protected EhisClient ehisClient;
    @Autowired
    protected EhisLogService ehisLogService;
    @Autowired
    protected EntityManager em;

    @Value("${ehis.endpoint}")
    protected String endpoint;

    @Value("${ehis.user}")
    protected String user;

    @Value("${ehis.client.xRoadInstance}")
    protected String clientXRoadInstance;
    @Value("${ehis.client.memberClass}")
    protected String clientMemberClass;
    @Value("${ehis.client.memberCode}")
    protected String clientMemberCode;
    @Value("${ehis.client.subsystemCode}")
    protected String clientSubsystemCode;

    @Value("${ehis.service.xRoadInstance}")
    protected String serviceXRoadInstance;
    @Value("${ehis.service.memberClass}")
    protected String serviceMemberClass;
    @Value("${ehis.service.memberCode}")
    protected String serviceMemberCode;
    @Value("${ehis.service.subsystemCode}")
    protected String serviceSubsystemCode;

    @PostConstruct
    public void postConstruct() {
        try {
            datatypeFactory = DatatypeFactory.newInstance();
        } catch (DatatypeConfigurationException e) {
            throw new BadConfigurationException("Unable to create data type factory", e);
        }
    }

    protected WsEhisStudentLog laeKorgharidused(KhlOppeasutusList khlOppeasutusList, WsEhisStudentLog wsEhisStudentLog) {
        XRoadHeaderV4 xRoadHeaderV4 = getXroadHeader();
        EhisLaeKorgharidusedResponse response = ehisClient.laeKorgharidused(xRoadHeaderV4, khlOppeasutusList);

        LogContext queryLog = response.getLog();
        wsEhisStudentLog.setHasXteeErrors(Boolean.valueOf(queryLog.getError() != null));

        if(!response.hasError()) {
            boolean error = messageHasError(response.getTeade()) || resultHasError(response.getResult());
            wsEhisStudentLog.setHasOtherErrors(Boolean.valueOf(error));
            wsEhisStudentLog.setLogTxt(String.join(";", StreamUtil.nullSafeList(response.getResult())));
        } else {
            wsEhisStudentLog.setHasXteeErrors(Boolean.TRUE);
            wsEhisStudentLog.setLogTxt(ExceptionUtil.getRootCause(queryLog.getError()).toString());
        }
        return ehisLogService.insert(queryLog, wsEhisStudentLog);
    }

    protected static KhlOppeasutusList getKhlOppeasutusList(Student student) {
        return getKhlOppeasutusList(student, khlOppeasutus -> khlOppeasutus.getOppur().add(getKhlOppurMuutmine(student, true)));
    }
    
    protected static KhlOppeasutusList getKhlOppeasutusList(Student student, Consumer<KhlOppeasutus> oppeasutusConsumer) {
        if (!StudentUtil.isGuestStudent(student)) {
            Curriculum curriculum = student.getCurriculumVersion().getCurriculum();
            if (Boolean.TRUE.equals(curriculum.getJoint()) && curriculum.getJointMentor() != null
                    && !student.getSchool().getEhisSchool().equals(curriculum.getJointMentor())) {
                throw new HoisException(TranslateUtil.translate("ehis.schoolIsNotJointMentor", Language.ET));
            }
        }
        
        KhlOppeasutusList khlOppeasutusList = new KhlOppeasutusList();
        KhlOppeasutus khlOppeasutus = new KhlOppeasutus();

        String koolId = ehisValue(student.getSchool().getEhisSchool());
        // FIXME strings are allowed values too
        khlOppeasutus.setKoolId(koolId != null ? new BigInteger(koolId) : null);

        oppeasutusConsumer.accept(khlOppeasutus);
        khlOppeasutusList.getOppeasutus().add(khlOppeasutus);
        return khlOppeasutusList;
    }
    
    protected static KhlOppeasutusList getKhlOppeasutusListGuestStudent(Student student) {
        KhlOppeasutusList khlOppeasutusList = new KhlOppeasutusList();
        KhlOppeasutus khlOppeasutus = new KhlOppeasutus();

        String koolId = ehisValue(student.getSchool().getEhisSchool());
        // EHIS_KOOL classifiers should only have school Id's in ehis_value field
        khlOppeasutus.setKoolId(koolId != null ? new BigInteger(koolId) : null);
        khlOppeasutusList.getOppeasutus().add(khlOppeasutus);
        return khlOppeasutusList;
    }

    protected static KhlOppur getKhlOppurMuutmine(Student student, boolean setOppekava) {
        KhlOppur khlOppur = new KhlOppur();
        KhlMuutmine muutmine = new KhlMuutmine();
        Person person = student.getPerson();

        String personId = getPersonId(person);
        muutmine.setIsikukood(personId);

        if (person.getIdcode() == null) {
            muutmine.setKlIsikukoodRiik(BIRTH_DATE_ENTERED);
        }

        if (setOppekava) {
            muutmine.setOppekava(requiredCurriculumCode(student));
        }
        khlOppur.setMuutmine(muutmine);
        return khlOppur;
    }
    
    protected static KhlOppur getKhlOppurDuplikaadiMuutmine(Student student, boolean setOppekava) {
        KhlOppur khlOppur = new KhlOppur();
        KhlDuplikaadiMuutmine muutmine = new KhlDuplikaadiMuutmine();
        Person person = student.getPerson();

        String personId = getPersonId(person);
        muutmine.setIsikukood(personId);

        if (person.getIdcode() == null) {
            muutmine.setKlIsikukoodRiik(BIRTH_DATE_ENTERED);
        }

        if (setOppekava) {
            muutmine.setOppekava(requiredCurriculumCode(student));
        }
        khlOppur.setDuplikaadiMuutmine(muutmine);
        return khlOppur;
    }

    protected KhlOppur getKhlOppurLisamine(Student student) {
        Person person = student.getPerson();
        KhlIsikuandmedLisa isikuandmedLisa = new KhlIsikuandmedLisa();
        isikuandmedLisa.setIsikukood(getPersonId(person));

        if (person.getIdcode() == null) {
            isikuandmedLisa.setSynniKp(date(person.getBirthdate()));
            isikuandmedLisa.setKlIsikukoodRiik(BIRTH_DATE_ENTERED);
            if (person.getSex() != null) {
                isikuandmedLisa.setKlSugu(ehisValue(person.getSex()));
            }
            if ((person.getFirstname() != null) && !person.getFirstname().isEmpty()) {
                isikuandmedLisa.setEesnimi(person.getFirstname());
            }
            if ((person.getLastname() != null) && !person.getLastname().isEmpty()) {
                isikuandmedLisa.setPerenimi(person.getLastname());
            }
            if (person.getCitizenship() != null) {
                // XXX citizenship is three letters?
                isikuandmedLisa.setKlKodakondsus(value(person.getCitizenship()));
            }
            if (person.getResidenceCountry() != null) {
                isikuandmedLisa.setKlElukohamaa(value2(person.getResidenceCountry()));
            }
        }

        KhlLisamine lisamine = new KhlLisamine();
        lisamine.setIsikuandmed(isikuandmedLisa);
        KhlOppur khlOppur = new KhlOppur();
        khlOppur.setLisamine(lisamine);
        return khlOppur;
    }

    protected static BigInteger requiredCurriculumCode(Curriculum curriculum, Student student) {
        BigInteger code = curriculumCode(curriculum);
        if (code == null) {
            throw new ValidationFailedException("Õppija " + getPersonId(student.getPerson()) + " õppekaval puudub HTM kood");
        }
        return code;
    }

    protected static BigInteger requiredCurriculumCode(Student student) {
        return requiredCurriculumCode(student.getCurriculumVersion().getCurriculum(), student);
    }

    protected XMLGregorianCalendar date(LocalDate date) {
        if(date == null) {
            return null;
        }
        return datatypeFactory.newXMLGregorianCalendar(date.toString());
    }

    protected static String code(Classifier classifier) {
        return EntityUtil.getNullableCode(classifier);
    }
    
    protected static BigInteger bigInt(Number number) {
        return number != null ? BigInteger.valueOf(number.longValue()) : null;
    }

    protected static String ehisValue(Classifier classifier) {
        return classifier != null ? classifier.getEhisValue() : null;
    }

    protected static String name(Classifier classifier) {
        return classifier != null ? classifier.getNameEt() : null;
    }
    
    public static String name(Translatable translatable) {
        return translatable != null ? translatable.getNameEt() : null;
    }

    protected static String value(Classifier classifier) {
        return classifier != null ? classifier.getValue() : null;
    }

    protected static String value2(Classifier classifier) {
        return classifier != null ? classifier.getValue2() : null;
    }

    protected static String yesNo(Boolean value) {
        return Boolean.TRUE.equals(value) ? "jah" : "ei";
    }

    protected static BigInteger curriculumCode(Curriculum curriculum) {
        String merCode = curriculum.getMerCode();
        return merCode != null ? new BigInteger(merCode) : null;
    }

    private static String getPersonId(Person person) {
        return (person.getIdcode() != null) ? person.getIdcode() :
                ((person.getForeignIdcode() != null) ? person.getForeignIdcode() :
                        person.getBirthdate().toString());
    }

    protected WsEhisStudentLog bindingException(Directive directive, Exception e) {
        WsEhisStudentLog studentLog = baseBindingException(e);
        studentLog.setDirective(directive);
        studentLog.setSchool(directive.getSchool());
        LogContext logContext = new LogContext(null, LAE_KORGHARIDUS_SERVICE);
        logContext.setError(e);
        return ehisLogService.insert(logContext, studentLog);
    }

    protected WsEhisStudentLog bindingException(Student student, Exception e) {
        return bindingException(student, e, LAE_KORGHARIDUS_SERVICE);
    }
    
    protected WsEhisStudentLog bindingException(Student student, Exception e, String wsName) {
        WsEhisStudentLog studentLog = baseBindingException(e);
        studentLog.setSchool(student.getSchool());
        LogContext logContext = new LogContext(null, wsName);
        logContext.setError(e);
        return ehisLogService.insert(logContext, studentLog);
    }

    private static WsEhisStudentLog baseBindingException(Exception e) {
        WsEhisStudentLog studentLog = new WsEhisStudentLog();
        studentLog.setHasOtherErrors(Boolean.TRUE);
        studentLog.setLogTxt(e.toString());
        log.error("Binding failed: ", e);
        return studentLog;
    }

    protected XRoadHeaderV4 getXroadHeader() {
        XRoadHeaderV4.Client client = new XRoadHeaderV4.Client();
        client.setXRoadInstance(clientXRoadInstance);
        client.setMemberClass(clientMemberClass);
        client.setMemberCode(clientMemberCode);
        client.setSubSystemCode(clientSubsystemCode);

        XRoadHeaderV4.Service service = new XRoadHeaderV4.Service();
        service.setxRoadInstance(serviceXRoadInstance);
        service.setMemberClass(serviceMemberClass);
        service.setMemberCode(serviceMemberCode);
        service.setServiceCode(getServiceCode());
        service.setSubsystemCode(serviceSubsystemCode);

        XRoadHeaderV4 header = new XRoadHeaderV4();
        header.setClient(client);
        header.setService(service);
        header.setEndpoint(endpoint);
        header.setUserId(user);
        return header;
    }

    protected abstract String getServiceCode();

    /**
     * errors are reported as success. If any of item has magic string Viga then consider result as error
     * @param result
     * @return
     */
    protected boolean resultHasError(List<String> result) {
        return StreamUtil.nullSafeList(result).stream().anyMatch(r -> r != null && r.contains(ERROR_MARKER_RESULT));
    }

    protected boolean messageHasError(String msg) {
        return msg == null || msg.contains(ERROR_MARKER_MSG);
    }
}
