package ee.hitsa.ois.service.ekis;

import static ee.hitsa.ois.util.JpaQueryUtil.resultAsString;

import java.lang.invoke.MethodHandles;
import java.math.BigDecimal;
import java.text.DecimalFormat;
import java.text.DecimalFormatSymbols;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.UUID;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import javax.persistence.EntityManager;
import javax.transaction.Transactional;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Profile;
import org.springframework.stereotype.Service;
import org.springframework.util.StringUtils;

import ee.hitsa.ois.domain.BaseEntityWithId;
import ee.hitsa.ois.domain.Certificate;
import ee.hitsa.ois.domain.Classifier;
import ee.hitsa.ois.domain.Contract;
import ee.hitsa.ois.domain.ContractModuleSubject;
import ee.hitsa.ois.domain.ContractSupervisor;
import ee.hitsa.ois.domain.Person;
import ee.hitsa.ois.domain.WsEkisLog;
import ee.hitsa.ois.domain.curriculum.Curriculum;
import ee.hitsa.ois.domain.curriculum.CurriculumVersion;
import ee.hitsa.ois.domain.directive.Directive;
import ee.hitsa.ois.domain.directive.DirectiveCoordinator;
import ee.hitsa.ois.domain.directive.DirectiveStudent;
import ee.hitsa.ois.domain.directive.DirectiveStudentDuplicateForm;
import ee.hitsa.ois.domain.enterprise.Enterprise;
import ee.hitsa.ois.domain.scholarship.ScholarshipApplication;
import ee.hitsa.ois.domain.school.School;
import ee.hitsa.ois.domain.student.Student;
import ee.hitsa.ois.domain.student.StudentGroup;
import ee.hitsa.ois.domain.student.StudentRepresentative;
import ee.hitsa.ois.domain.teacher.Teacher;
import ee.hitsa.ois.enums.CertificateStatus;
import ee.hitsa.ois.enums.CertificateType;
import ee.hitsa.ois.enums.ContractStatus;
import ee.hitsa.ois.enums.DirectiveStatus;
import ee.hitsa.ois.enums.DirectiveType;
import ee.hitsa.ois.enums.HigherAssessment;
import ee.hitsa.ois.enums.OccupationalGrade;
import ee.hitsa.ois.enums.ProtocolStatus;
import ee.hitsa.ois.repository.PersonRepository;
import ee.hitsa.ois.service.StudentAbsenceService;
import ee.hitsa.ois.util.DateUtils;
import ee.hitsa.ois.util.DirectiveUtil;
import ee.hitsa.ois.util.EntityUtil;
import ee.hitsa.ois.util.JpaNativeQueryBuilder;
import ee.hitsa.ois.util.PersonUtil;
import ee.hitsa.ois.util.StreamUtil;
import ee.hitsa.ois.util.StudentUtil;
import ee.hitsa.ois.util.Translatable;
import ee.hitsa.ois.validation.ValidationFailedException;
import ee.hitsa.ois.web.dto.AutocompleteResult;
import ee.hois.soap.LogResult;
import ee.hois.soap.ekis.client.DeleteDirectiveRequest;
import ee.hois.soap.ekis.client.EkisClient;
import ee.hois.soap.ekis.client.EkisRequestContext;
import ee.hois.soap.ekis.client.RegisterCertificateRequest;
import ee.hois.soap.ekis.client.RegisterDirectiveRequest;
import ee.hois.soap.ekis.client.RegisterPracticeContractRequest;
import ee.hois.soap.ekis.client.generated.Content;

@Transactional
@Service
@Profile("!test")
public class EkisService {

    private static final Logger LOG = LoggerFactory.getLogger(MethodHandles.lookup().lookupClass());
    // we want to send always dot as decimal separator
    private static final DecimalFormatSymbols MONEY_FORMAT_SYMBOLS = DecimalFormatSymbols.getInstance(Locale.ROOT);
    
    private static final String GUEST_STUDENT = "Külalisõppija";
    
    private static final String GUEST_STUDENT_COURSE = "1";
    
    static {
        MONEY_FORMAT_SYMBOLS.setDecimalSeparator('.');
    }
    // format class is not thread-safe
    private static final ThreadLocal<DecimalFormat> MONEY_FORMAT = ThreadLocal.withInitial(() -> new DecimalFormat("0.00", MONEY_FORMAT_SYMBOLS));

    // XXX we send 0 as missing wd_id value
    private static final int MISSING_WD_ID = 0;
    // XXX we send 0 as missing int value
    private static final int MISSING_INT = 0;

    @Autowired
    private EkisClient ekis;
    @Autowired
    private EkisLogService ekisLogService;
    @Autowired
    private StudentAbsenceService studentAbsenceService;
    @Autowired
    private PersonRepository personRepository;
    @Autowired
    protected EntityManager em;
    @Value("${ekis.endpoint}")
    protected String endpoint;

    /**
     * Send register certificate request to EKIS
     *
     * @param certificateId
     * @return certificate with status updated, certificate nr and wd_id set
     * @throws ValidationFailedException if there was an error
     */
    public Certificate registerCertificate(Long certificateId) {
        Certificate certificate = em.getReference(Certificate.class, certificateId);

        RegisterCertificateRequest request = new RegisterCertificateRequest();
        request.setQguid(qguid());
        request.setEhisId(certificate.getSchool().getEhisSchool().getEhisValue());
        request.setOisId(certificate.getId().toString());

        Person person;
        if(CertificateType.isOther(EntityUtil.getCode(certificate.getType())) && certificate.getStudent() == null) {
            person = personRepository.findByIdcode(certificate.getOtherIdcode());
        } else {
            person = certificate.getStudent().getPerson();
        }
        if(person != null) {
            request.setStudent(person.getFullname());
            request.setEmail(certificate.getStudent() != null && StringUtils.hasText(certificate.getStudent().getEmail())
                    ? certificate.getStudent().getEmail() : person.getEmail());
        } else {
            request.setStudent(certificate.getOtherName());
        }
        request.setSubject(certificate.getHeadline());
        request.setBody(certificate.getContent());
        request.setItemCreator(certificate.getSignatoryIdcode());
        request.setType(value(certificate.getType()));
        request.setInstitution(certificate.getWhom());

        return withResponse(ekis.registerCertificate(ctx(certificate.getSchool()), request), (result) -> {
            certificate.setWdId(Long.valueOf(result.getWdId()));
            // TODO waiting for implementation in EKIS
            // certificate.setWdUrl(result.getWdUrl());
            certificate.setCertificateNr(result.getRegno());
            certificate.setStatus(em.getReference(Classifier.class, CertificateStatus.TOEND_STAATUS_V.name()));
            return save(certificate);
        }, certificate.getSchool(), certificate, l -> l.setCertificate(certificate));
    }

    /**
     * Send register directive request to EKIS
     *
     * @param directiveId
     * @return directive with status update and wd_id set
     * @throws ValidationFailedException if there was an error
     */
    public Directive registerDirective(Long directiveId) {
        Directive directive = em.getReference(Directive.class, directiveId);

        RegisterDirectiveRequest request = new RegisterDirectiveRequest();
        request.setQguid(qguid());
        request.setEhisId(ehisId(directive.getSchool()));
        request.setOisId(directive.getId().toString());
        request.setDirectiveType(value(directive.getType()));
        request.setTitle(directive.getHeadline());
        DirectiveCoordinator manager = directive.getDirectiveCoordinator();
        // XXX item creator same as manager
        request.setItemCreator(manager != null ? manager.getIdcode() : null);
        request.setCreateTime(date(directive.getInserted()));
        request.setManager(manager != null ? manager.getIdcode() : null);
        DirectiveType directiveType = DirectiveType.valueOf(EntityUtil.getCode(directive.getType()));
        directive.getStudents().sort(DirectiveUtil.getStudentEkisComparator(directiveType));
        request.setContent(StreamUtil.toMappedList(ds -> studentForRegisterDirective(directiveType, ds), directive.getStudents()));
        request.setWdId(directive.getWdId() != null ? directive.getWdId().intValue() : MISSING_WD_ID);

        return withResponse(ekis.registerDirective(ctx(directive.getSchool()), request), (result) -> {
            directive.setWdId(Long.valueOf(result.getWdId()));
            directive.setStatus(em.getReference(Classifier.class, DirectiveStatus.KASKKIRI_STAATUS_KINNITAMISEL.name()));
            return save(directive);
        }, directive.getSchool(), directive, l -> l.setDirective(directive));
    }

    /**
     * send delete directive request to EKIS
     *
     * @param directiveId
     * @throws ValidationFailedException if there was an error
     */
    public void deleteDirective(Long directiveId) {
        Directive directive = em.getReference(Directive.class, directiveId);

        DeleteDirectiveRequest request = new DeleteDirectiveRequest();
        request.setQguid(qguid());
        request.setOisId(directive.getId().toString());
        request.setWdId(directive.getWdId().intValue());

        withResponse(ekis.deleteDirective(ctx(directive.getSchool()), request), (result) -> {
            // do nothing
            return null;
        }, directive.getSchool(), directive, l -> {});
    }

    /**
     * send register practice contract request to EKIS
     *
     * @param contractId
     * @return contract with status updated and wd_id set
     * @throws ValidationFailedException if there was an error
     */
    public Contract registerPracticeContract(Long contractId) {
        Contract contract = em.getReference(Contract.class, contractId);

        RegisterPracticeContractRequest request = new RegisterPracticeContractRequest();
        request.setQguid(qguid());
        request.setEhisId(ehisId(contract.getStudent().getSchool()));
        request.setOisId(contract.getId().toString());
        request.setManager(contract.getContractCoordinator() != null ? contract.getContractCoordinator().getIdcode() : null);

        Student student = contract.getStudent();
        Person person = student.getPerson();
        Teacher tutor = contract.getTeacher();
        String idcode = null;
        boolean higher = StudentUtil.isHigher(student);
        boolean guestStudent = StudentUtil.isGuestStudent(student);
        
        if (tutor != null) {
            Person tutorPerson = tutor.getPerson();
            if (tutorPerson != null) {
                request.setSchTutorFirstName(tutorPerson.getFirstname());
                request.setSchTutorLastName(tutorPerson.getLastname());
                request.setSchoolTutorId(tutorPerson.getIdcode());
                String schTutorPhone = tutor.getPhone();
                if (schTutorPhone != null) {
                    request.setSchTutorPhone(schTutorPhone);
                } else {
                    request.setSchTutorPhone(tutorPerson.getPhone());
                }
                String schTutorEmail = tutor.getEmail();
                if (schTutorEmail != null) {
                    request.setSchTutorEmail(schTutorEmail);
                } else {
                    request.setSchTutorEmail(tutorPerson.getEmail());
                }
            }
        }
        
        if(StringUtils.hasText(person.getIdcode())) {
            idcode = person.getIdcode();
        } else if(person.getBirthdate() != null) {
            idcode = DateUtils.date(person.getBirthdate());
        }
        request.setStIdCode(idcode);
        request.setStFirstNames(person.getFirstname());
        request.setStLastName(person.getLastname());
        request.setStPhone(person.getPhone());
        request.setStPostcode(person.getPostcode());
        request.setStAddress(person.getAddress());
        request.setStAddressAds(person.getAddressAds());
        request.setStAddressAdsOid(person.getAddressAdsOid());
        Classifier residenceCountry = person.getResidenceCountry();
        if (residenceCountry != null) {
            request.setStResidenceCountry(residenceCountry.getNameEt());
        }
        String studentEmail = student.getEmail();
        if (studentEmail == null) {
            throw new ValidationFailedException("contract.messages.sendToEkis.error.studentEmailMissing");
        }
        request.setStEmail(studentEmail);
        
        CurriculumVersion curriculumVersion = student.getCurriculumVersion();
        if (curriculumVersion != null) {
            request.setStCurricula(curriculumVersion.getCurriculum().getNameEt());
        } else if (guestStudent) {
            request.setStCurricula(GUEST_STUDENT);
        }
        
        Classifier studyForm = student.getStudyForm();
        if (studyForm != null) {
            request.setStForm(studyForm.getNameEt());
        } else if (higher && guestStudent) {
            request.setStForm(em.getReference(Classifier.class, "OPPEVORM_P").getNameEt());
        } else if (!higher && guestStudent) {
            request.setStForm(em.getReference(Classifier.class, "OPPEVORM_Z").getNameEt());
        }

        StudentGroup sg = student.getStudentGroup();
        String course = sg != null ? sg.getCourse().toString() : null;
        if (course == null && !guestStudent) {
            throw new ValidationFailedException("contract.messages.sendToEkis.error.studentGroupCourseMissing");
        } else if (course == null && guestStudent) {
            course = GUEST_STUDENT_COURSE;
        }
        request.setStCourse(course);
        
        List<ContractModuleSubject> moduleSubjects = contract.getModuleSubjects();
        Stream<String> moduleStream = null;
        Stream<String> aimStream = null;
        Stream<String> outcomesStream = null;
        if (higher) {
            moduleStream = moduleSubjects.stream().map(ms -> ms.getSubject().getNameEt());
            Comparator<ContractModuleSubject> contractModuleSubjectComparator = 
                    (ContractModuleSubject o1, ContractModuleSubject o2) -> {
                        String o1String = o1.getSubject().getNameEt();
                        String o2String = o2.getSubject().getNameEt();
                        return o1String.compareTo(o2String);
                    };
            Collections.sort(moduleSubjects, contractModuleSubjectComparator);
            aimStream = moduleSubjects.stream().map(ms -> {
                if (ms.getSubject() == null) return null;
                return ms.getSubject().getObjectivesEt();
            });
            outcomesStream = moduleSubjects.stream().map(ms -> {
                if (ms.getSubject() == null) return null;
                return ms.getSubject().getOutcomesEt();
            });
        } else {
            moduleStream = moduleSubjects.stream().map(ms -> {
                List<String> names = Arrays.asList(ms.getModule().getCurriculumModule().getNameEt(), 
                        ms.getTheme() != null ? ms.getTheme().getNameEt() : null);
                return names.stream().filter(r -> r != null).collect(Collectors.joining(", "));
            });
            Comparator<ContractModuleSubject> contractModuleSubjectComparator = 
                    (ContractModuleSubject o1, ContractModuleSubject o2) -> {
                        String o1String = Arrays.asList(o1.getModule().getCurriculumModule().getNameEt(), 
                                o1.getTheme() != null ? o1.getTheme().getNameEt() : null).stream().filter(r -> r != null).collect(Collectors.joining(", "));
                        String o2String = Arrays.asList(o2.getModule().getCurriculumModule().getNameEt(), 
                                o2.getTheme() != null ? o2.getTheme().getNameEt() : null).stream().filter(r -> r != null).collect(Collectors.joining(", "));
                        return o1String.compareTo(o2String);
                    };
            Collections.sort(moduleSubjects, contractModuleSubjectComparator);
            aimStream = moduleSubjects.stream().map(ms -> ms.getModule().getCurriculumModule().getObjectivesEt());
            outcomesStream = moduleSubjects.stream().map(ms -> {
                if (ms.getModule() == null 
                        || ms.getModule().getCurriculumModule() == null
                        || ms.getModule().getCurriculumModule().getOutcomes() == null) return null;
                List<String> outcomes = ms.getModule().getCurriculumModule().getOutcomes()
                        .stream().map(p -> p.getOutcomeEt()).collect(Collectors.toList());
                return outcomes.stream().filter(r -> r != null).collect(Collectors.joining(", "));
            });
        }
        request.setStModule(moduleStream.sorted(String.CASE_INSENSITIVE_ORDER).collect(Collectors.joining(" &&& ")));
        request.setStEkap(moduleSubjects.stream().filter(p -> p.getCredits() != null).map(p -> p.getCredits().toString()).collect(Collectors.joining(" &&& ")));
        request.setStHours(moduleSubjects.stream().filter(p -> p.getHours() != null).map(p -> p.getHours().toString()).collect(Collectors.joining(" &&& ")));
        Enterprise enterprise = contract.getEnterprise();
        request.setOrgName(enterprise.getName());
        request.setOrgCode(enterprise.getRegCode());
        request.setOrgContactName(contract.getContactPersonName());
        request.setOrgTel(contract.getContactPersonPhone());
        request.setOrgEmail(contract.getContactPersonEmail());
        List<ContractSupervisor> supervisors = contract.getContractSupervisors();
        Comparator<ContractSupervisor> contractSupervisorComparator = 
                (ContractSupervisor o1, ContractSupervisor o2) -> o1.getInserted().compareTo(o2.getInserted());
        Collections.sort(supervisors, contractSupervisorComparator);
        if (supervisors.size() > 0) {
            ContractSupervisor orgTutor = supervisors.get(0);
            request.setOrgTutorName(orgTutor.getSupervisorName());
            request.setOrgTutorTel(orgTutor.getSupervisorPhone());
            request.setOrgTutorEmail(orgTutor.getSupervisorEmail());
        }
        if (supervisors.size() > 1) {
            ContractSupervisor orgTutor = supervisors.get(1);
            request.setOrgTutor2Name(orgTutor.getSupervisorName());
            request.setOrgTutor2Tel(orgTutor.getSupervisorPhone());
            request.setOrgTutor2Email(orgTutor.getSupervisorEmail());
        }
        if (supervisors.size() > 2) {
            ContractSupervisor orgTutor = supervisors.get(2);
            request.setOrgTutor3Name(orgTutor.getSupervisorName());
            request.setOrgTutor3Tel(orgTutor.getSupervisorPhone());
            request.setOrgTutor3Email(orgTutor.getSupervisorEmail());
        }
        request.setProgramme(contract.getPracticePlan());
        request.setOutcomes(outcomesStream.filter(p -> p != null).collect(Collectors.joining(", ")));
        request.setAim(aimStream.filter(p -> p != null).collect(Collectors.joining(", ")));
        request.setStartDate(date(contract.getStartDate()));
        request.setEndDate(date(contract.getEndDate()));
        request.setPlace(contract.getPracticePlace());
        if (request.getPlace() == null) {
            if (contract.getIsPracticeSchool() != null && contract.getIsPracticeSchool().booleanValue()) {
                request.setPlace("Praktika sooritatakse koolis");
            } else if (contract.getIsPracticeTelework() != null && contract.getIsPracticeTelework().booleanValue()) {
                request.setPlace("Praktika sooritatakse kaugtööna");
            }
        }

        return withResponse(ekis.registerPracticeContract(ctx(contract.getStudent().getSchool()), request), (result) -> {
            contract.setWdId(Long.valueOf(result.getWdId()));
            contract.setStatus(em.getReference(Classifier.class, ContractStatus.LEPING_STAATUS_Y.name()));
            if (Boolean.TRUE.equals(contract.getIsPracticeAbsence()) || Boolean.TRUE.equals(contract.getIsPracticeHidden())) {
                studentAbsenceService.createContractAbsence(contract);
            }
            return save(contract);
        }, contract.getStudent().getSchool(), contract, l -> l.setContract(contract));
    }

    protected <T extends BaseEntityWithId> T save(T entity) {
        return EntityUtil.save(entity, em);
    }

    private Content studentForRegisterDirective(DirectiveType directiveType, DirectiveStudent ds) {
        Content content = new Content();
        Person person = ds.getPerson();
        Student student = ds.getStudent();

        String idcode = null;
        if(StringUtils.hasText(person.getIdcode())) {
            idcode = person.getIdcode();
        } else if(person.getBirthdate() != null) {
            idcode = DateUtils.date(person.getBirthdate());
        }
        content.setIdCode(idcode);

        content.setFirstName(person.getFirstname());
        content.setLastName(person.getLastname());
        if (student != null) {
            if (Boolean.TRUE.equals(student.getIsRepresentativeMandatory()) || !PersonUtil.isAdult(person)) {
                List<StudentRepresentative> representatives = student.getRepresentatives();
                if (representatives != null) {
                    representatives.stream()
                    .filter(r -> Boolean.TRUE.equals(r.getIsStudentVisible()))
                    .sorted(Comparator.comparing(r -> EntityUtil.getCode(r.getRelation()), Comparator.reverseOrder()))
                    .findFirst().ifPresent(r -> {
                        Person representative = r.getPerson();
                        content.setReprFirstName(representative.getFirstname());
                        content.setReprLastName(representative.getLastname());
                    });
                }
            }
            content.setCurricula(curriculum(student));
            CurriculumVersion curriculumVersion = student.getCurriculumVersion();
            if (curriculumVersion != null) {
                Curriculum curriculum = curriculumVersion.getCurriculum();
                content.setCurriculaMerCode(curriculum.getMerCode());
                content.setStudyLevel(name(curriculum.getOrigStudyLevel()));
                content.setCurriculaStudyPeriod(intValue(curriculum.getStudyPeriod()));
                content.setForm(name(student.getStudyForm()));
            }
            StudentGroup studentGroup = ds.getStudentGroup();
            if (studentGroup == null) {
                studentGroup = student.getStudentGroup();
            }
            if (studentGroup != null) {
                content.setGroup(studentGroup.getCode());
                content.setCourse(intValue(studentGroup.getCourse()));
                Teacher groupTeacher = studentGroup.getTeacher();
                if (groupTeacher != null) {
                    Person teacher = groupTeacher.getPerson();
                    content.setTeacherFirstName(teacher.getFirstname());
                    content.setTeacherLastName(teacher.getLastname());
                }
            }
            content.setFinsource(name(student.getFin()));
            content.setLang(name(student.getLanguage()));
        }
        switch(directiveType) {
        case KASKKIRI_AKAD:
            content.setStartDate(periodStart(ds));
            content.setEndDate(periodEnd(ds));
            content.setReason(name(ds.getReason()));
            break;
        case KASKKIRI_AKADK:
            content.setEndDate(date(ds.getStartDate()));
            break;
        case KASKKIRI_DUPLIKAAT:
            List<String> codes = new ArrayList<>();
            if (ds.getDiplomaForm() != null) {
                codes.add(ds.getDiplomaForm().getFullCode());
            }

            List<DirectiveStudentDuplicateForm> forms = new ArrayList<>();
            List<DirectiveStudentDuplicateForm> formsEn = new ArrayList<>();
            ds.getForms().forEach(f -> (Boolean.TRUE.equals(f.getEn()) ? formsEn : forms).add(f));
            
            if (ds.getDiplomaSupplement() != null) {
                forms.forEach(dsForm -> codes.add(dsForm.getForm().getFullCode()));
            }
            if (ds.getDiplomaSupplementEn() != null) {
                formsEn.forEach(dsForm -> codes.add(dsForm.getForm().getFullCode()));
            }
            
            content.setReason(String.join(", ", codes) + "???" + ds.getAddInfo());
            break;
        case KASKKIRI_EKSMAT:
            content.setReason(name(ds.getReason()));
            break;
        case KASKKIRI_ENNIST:
            // only student group is edited, take other values from student
            content.setLoad(name(student.getStudyLoad()));
            content.setForm(name(student.getStudyForm()));
            content.setCurricula(curriculum(student));
            content.setGroup(studentGroup(ds));
            content.setFinsource(name(student.getFin()));
            content.setLang(name(student.getLanguage()));
            break;
        case KASKKIRI_FINM:
            content.setFinsource(finsource(ds.getFin(), ds.getFinSpecific()));
            break;
        case KASKKIRI_IMMAT:
        case KASKKIRI_IMMATV:
            content.setLoad(name(ds.getStudyLoad()));
            content.setForm(name(ds.getStudyForm()));
            content.setCurricula(curriculum(ds));
            content.setGroup(studentGroup(ds));
            content.setFinsource(name(ds.getFin()));
            content.setLang(name(ds.getLanguage()));
            break;
        case KASKKIRI_INDOK:
            content.setStartDate(date(ds.getStartDate()));
            content.setEndDate(date(ds.getEndDate()));
            break;
        case KASKKIRI_INDOKLOP:
            content.setEndDate(date(ds.getStartDate()));
            break;
        case KASKKIRI_LOPET:
            boolean higher = StudentUtil.isHigher(student);

            content.setCurricula(curriculum(ds));
            content.setKudos(yesNo(ds.getIsCumLaude()));
            content.setDegree(name(ds.getCurriculumGrade()));

            JpaNativeQueryBuilder qb = new JpaNativeQueryBuilder("from protocol_student ps "
                    + "join protocol_student_occupation pso on pso.protocol_student_id = ps.id "
                    + "join classifier pso_cl on pso_cl.code = pso.occupation_code "
                    + "left join classifier pso_cl2 on pso_cl2.code = pso.part_occupation_code "
                    + "join protocol p on p.id = ps.protocol_id "
                    + "left join student_occupation_certificate soc on soc.id = pso.student_occupation_certificate_id "
                    + "left join classifier soc_cl on soc_cl.code = soc.speciality_code");

            qb.requiredCriteria("ps.student_id = :studentId", "studentId", EntityUtil.getId(student));
            qb.filter("p.is_final = true");
            qb.requiredCriteria("p.status_code = :pstatus", "pstatus", ProtocolStatus.PROTOKOLL_STAATUS_K.name());
            qb.requiredCriteria("ps.grade_code in :grades", "grades", higher ?
                    Stream.of(HigherAssessment.values()).filter(HigherAssessment::getIsPositive)
                        .map(HigherAssessment::name).collect(Collectors.toList())
                    : OccupationalGrade.OCCUPATIONAL_GRADE_POSITIVE);

            qb.groupBy("pso_cl.name_et, pso_cl2.name_et");
            qb.sort("pso_cl.name_et, pso_cl2.name_et");
            List<?> data = qb.select(" coalesce(pso_cl2.name_et, pso_cl.name_et) occupation "
                    + ", string_agg(soc_cl.name_et, ',') specialization", em).setMaxResults(1).getResultList();

            if (!data.isEmpty()) {
                String occupation = resultAsString(data.get(0), 0);
                String specialization = resultAsString(data.get(0), 1);
                content.setOccupation(occupation);
                content.setSpecialization(specialization != null ? specialization : null);
            }
            break;
        case KASKKIRI_OKAVA:
            content.setForm(name(ds.getStudyForm()));
            content.setCurricula(curriculum(ds));
            content.setGroup(studentGroup(ds));
            break;
        case KASKKIRI_OKOORM:
            content.setLoad(name(ds.getStudyLoad()));
            break;
        case KASKKIRI_OVORM:
            content.setForm(name(ds.getStudyForm()));
            content.setGroup(studentGroup(ds));
            break;
        case KASKKIRI_STIPTOET:
            // take curriculum and load from student
            // TODO study year
            content.setLoad(name(student.getStudyLoad()));
            content.setCurricula(curriculum(student));
            content.setStartDate(date(ds.getStartDate()));
            content.setEndDate(date(ds.getEndDate()));
            content.setStipType(value(ds.getDirective().getScholarshipType()));
            content.setStipName(name(ds.getDirective().getScholarshipType()));
            ScholarshipApplication scholarshipApplication = ds.getScholarshipApplication();
            if (scholarshipApplication != null) {
                BigDecimal grade = scholarshipApplication.getLastPeriodMark();
                if (grade == null) {
                    grade = scholarshipApplication.getAverageMark();
                }
                if (grade != null) {
                    content.setAvgGrade(grade.toString());
                }
            }
            content.setStipAmount(money(ds.getAmountPaid()));
            break;
        case KASKKIRI_STIPTOETL:
            content.setStartDate(date(ds.getStartDate()));
            content.setEndDate(date(ds.getEndDate()));
            content.setReason(name(ds.getReason()));
            content.setStipType(value(ds.getDirective().getScholarshipType()));
            content.setStipName(name(ds.getDirective().getScholarshipType()));
            break;
        case KASKKIRI_TUGI:
            content.setEndDate(date(ds.getEndDate()));
            content.setStartDate(date(ds.getStartDate()));
            break;
        case KASKKIRI_TUGILOPP:
            content.setEndDate(date(ds.getStartDate()));
            break;
        case KASKKIRI_VALIS:
            content.setStartDate(periodStart(ds));
            content.setEndDate(periodEnd(ds));
            content.setOuterschool(Boolean.TRUE.equals(ds.getIsAbroad()) ? ds.getAbroadSchool() : (ds.getEhisSchool() != null ? ds.getEhisSchool().getNameEt() : null));
            break;
        case KASKKIRI_KIITUS:
        case KASKKIRI_MUU:
        case KASKKIRI_NOOMI:
            content.setReason(ds.getAddInfo());
        case KASKKIRI_OTEGEVUS:
        case KASKKIRI_PRAKTIK:
            content.setStartDate(date(ds.getStartDate()));
            content.setEndDate(date(ds.getEndDate()));
            break;
        default:
            break;
        }
        return content;
    }

    private <T, R> R withResponse(LogResult<T> result, Function<T, R> handler, School school, R errorValue, Consumer<WsEkisLog> logCustomizer) {
        try {
            if(!result.hasError()) {
                return handler.apply(result.getResult());
            }
        } catch (Exception e) {
            result.getLog().setError(e);
            LOG.error("Error while handling EKIS response :", e);
        } finally {
            WsEkisLog logRecord = new WsEkisLog();
            logCustomizer.accept(logRecord);
            ekisLogService.insertLog(logRecord, school, result.getLog());

            if(result.hasError()) {
                Map<Object, Object> params = new HashMap<>();
                params.put("message", new AutocompleteResult(null, result.getLog().getError().getMessage(), result.getLog().getError().getMessage()));
                throw new ValidationFailedException("main.messages.error.ekis", params);
            }
        }
        return errorValue;
    }

    private static String curriculum(DirectiveStudent ds) {
        CurriculumVersion cv = ds.getCurriculumVersion();
        return cv != null ? cv.getCode() : null;
    }

    private static String curriculum(Student student) {
        CurriculumVersion cv = student.getCurriculumVersion();
        return cv != null ? cv.getCode() : null;
    }

    private static String date(LocalDateTime date) {
        return date != null ? date(date.toLocalDate()) : null;
    }

    private static String date(LocalDate date) {
        return date != null ? date.toString() : null;
    }

    private static int ehisId(School school) {
        try {
            if(school.getEhisSchool() != null) {
                return Integer.parseInt(school.getEhisSchool().getEhisValue(), 10);
            }
        } catch(@SuppressWarnings("unused") NumberFormatException e) {
            // ignore
        }
        return 0;
    }

    private static String money(BigDecimal amount) {
        return amount != null ? MONEY_FORMAT.get().format(amount) : null;
    }

    private static String name(Translatable object) {
        return object != null ? object.getNameEt() : null;
    }

    private static String periodStart(DirectiveStudent ds) {
        return date(DateUtils.periodStart(ds));
    }

    private static String periodEnd(DirectiveStudent ds) {
        return date(DateUtils.periodEnd(ds));
    }

    private static String studentGroup(DirectiveStudent ds) {
        StudentGroup sg = ds.getStudentGroup();
        return sg != null ? sg.getCode() : null;
    }

    private static String value(Classifier classifier) {
        return classifier != null ? classifier.getValue() : null;
    }

    private static int intValue(Number number) {
        return number != null ? number.intValue() : MISSING_INT;
    }
    
    private static String yesNo(Boolean value) {
        return value != null ? (Boolean.TRUE.equals(value) ? "jah" : "ei") : null;
    }
    
    private static String qguid() {
        return UUID.randomUUID().toString();
    }

    private EkisRequestContext ctx(School school) {
        return ctx(school.getEkisUrl() != null ? school.getEkisUrl() : this.endpoint);
    }

    private EkisRequestContext ctx(String endpoint) {
        return new EkisRequestContext(endpoint);
    }
    
    private static String finsource(Classifier fin, Classifier finSpecific) {
        return String.format("%s, %s - %s", name(fin), value(finSpecific), name(finSpecific)); 
    }
}
