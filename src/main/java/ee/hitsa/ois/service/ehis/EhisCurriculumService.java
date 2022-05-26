package ee.hitsa.ois.service.ehis;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.time.LocalDate;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.zip.ZipEntry;
import java.util.zip.ZipOutputStream;

import javax.transaction.Transactional;

import org.springframework.stereotype.Service;
import org.springframework.util.StringUtils;

import ee.hitsa.ois.domain.Classifier;
import ee.hitsa.ois.domain.OisFile;
import ee.hitsa.ois.domain.Person;
import ee.hitsa.ois.domain.WsEhisCurriculumLog;
import ee.hitsa.ois.domain.curriculum.Curriculum;
import ee.hitsa.ois.domain.curriculum.CurriculumAddress;
import ee.hitsa.ois.domain.curriculum.CurriculumFile;
import ee.hitsa.ois.domain.curriculum.CurriculumJointPartner;
import ee.hitsa.ois.domain.curriculum.CurriculumOccupation;
import ee.hitsa.ois.domain.curriculum.CurriculumOccupationSpeciality;
import ee.hitsa.ois.domain.curriculum.CurriculumStudyLanguage;
import ee.hitsa.ois.domain.curriculum.CurriculumVersion;
import ee.hitsa.ois.enums.MainClassCode;
import ee.hitsa.ois.exception.HoisException;
import ee.hitsa.ois.service.security.HoisUserDetails;
import ee.hitsa.ois.util.ClassifierUtil;
import ee.hitsa.ois.util.CurriculumUtil;
import ee.hitsa.ois.util.EntityUtil;
import ee.hitsa.ois.util.ExceptionUtil;
import ee.hitsa.ois.validation.ValidationFailedException;
import ee.hois.soap.LogContext;
import ee.hois.soap.LogResult;
import ee.hois.soap.SoapAttachment;
import ee.hois.xroad.ehis.generated.OisFail;
import ee.hois.xroad.ehis.generated.OisFailid;
import ee.hois.xroad.ehis.generated.OisInfoteade;
import ee.hois.xroad.ehis.generated.OisKutsestandard;
import ee.hois.xroad.ehis.generated.OisKutsestandardid;
import ee.hois.xroad.ehis.generated.OisOKSpetsialiseerumised;
import ee.hois.xroad.ehis.generated.OisOppekava;
import ee.hois.xroad.ehis.generated.OisOppekavaStaatus;
import ee.hois.xroad.ehis.generated.OisOppekavad;
import ee.hois.xroad.ehis.generated.OisOppekavadStaatus;
import ee.hois.xroad.ehis.generated.OisOppekeeled;
import ee.hois.xroad.ehis.generated.OisOsakutse;
import ee.hois.xroad.ehis.generated.OisToimumiseKohad;
import ee.hois.xroad.ehis.generated.OisValisOAS;
import ee.hois.xroad.ehis.generated.OisYhisOppekava;
import ee.hois.xroad.ehis.generated.OiskutseSpetsialiseerumine;
import ee.hois.xroad.ehis.generated.OppekavaOis;
import ee.hois.xroad.ehis.generated.OppekavaStaatusOis;
import ee.hois.xroad.ehis.service.EhisOisOppekavaResponse;
import ee.hois.xroad.ehis.service.EhisOisOppekavaStaatusResponse;
import ee.hois.xroad.helpers.XRoadHeaderV4;

@Transactional
@Service
public class EhisCurriculumService extends EhisService {

    private static final Pattern OCCUPATION_ID_PREFIX = Pattern.compile("^KUTSE_(\\d+)$");
    private static final Pattern PARTOCCUPATION_ID_PREFIX = Pattern.compile("^OSAKUTSE_(\\d+)$");

    private static final String OIS_OPPEKAVA_SERVICE_CODE = "oisOppekava";
    public static final String OIS_OPPEKAVA_SERVICE = "ehis."+ OIS_OPPEKAVA_SERVICE_CODE;
    private static final String OIS_OPPEKAVA_STAATUS_SERVICE_CODE = "oisOppekavaStaatus";
    public static final String OIS_OPPEKAVA_STAATUS_SERVICE = "ehis."+ OIS_OPPEKAVA_STAATUS_SERVICE_CODE;
    
    private static final String ZIP_FILE_NAME = "files.zip";

    private static final Map<String, String> CURRICULUM_STATUS_FROM_EHIS_STATUS = new HashMap<>();
    static {
        CURRICULUM_STATUS_FROM_EHIS_STATUS.put("OPPEKAVA_EHIS_STAATUS_R", "OPPEKAVA_STAATUS_K");
        CURRICULUM_STATUS_FROM_EHIS_STATUS.put("OPPEKAVA_EHIS_STAATUS_X", "OPPEKAVA_STAATUS_C");
        CURRICULUM_STATUS_FROM_EHIS_STATUS.put("OPPEKAVA_EHIS_STAATUS_S", "OPPEKAVA_STAATUS_S");
        CURRICULUM_STATUS_FROM_EHIS_STATUS.put("OPPEKAVA_EHIS_STAATUS_M", "OPPEKAVA_STAATUS_M");
        CURRICULUM_STATUS_FROM_EHIS_STATUS.put("OPPEKAVA_EHIS_STAATUS_L", "OPPEKAVA_STAATUS_M");
    }

    public void sendToEhis(HoisUserDetails userDetails, Curriculum curriculum, boolean isTest) {
        // TODO remove this block
        // EHIS oisOppekavad query is not working yet
        if(isTest) {
            if(!StringUtils.hasText(curriculum.getMerCode())) {
                throw new ValidationFailedException("curriculum.message.missingEhisCode");
            }
            return;
        }
        OisOppekava oisOppekava = new OisOppekava();
        oisOppekava.setRegNumber(value2(curriculum.getSchool().getEhisSchool()));
        Person person = em.getReference(Person.class, userDetails.getPersonId());
        oisOppekava.setKasutajaIk(person.getIdcode());
        OisOppekavad oppekavad = new OisOppekavad();
        oisOppekava.setOppekavad(oppekavad);

        boolean higher = Boolean.TRUE.equals(curriculum.getHigher());
        OppekavaOis oppekavaOis = new OppekavaOis();
        try {
            oppekavaOis.setOppekavaKood(curriculumCode(curriculum));
        } catch(@SuppressWarnings("unused") NumberFormatException e) {
            throw new ValidationFailedException("curriculum.message.badEhisCode");
        }
        oppekavaOis.setOppekavaLiik(higher ? "OK_LIIK_KORG" : "OK_LIIK_KUTSE");
        oppekavaOis.setOppekavaNimetus(curriculum.getNameEt());
        oppekavaOis.setOppekavaNimetusEng(curriculum.getNameEn());
        oppekavaOis.setTase(value(curriculum.getOrigStudyLevel()));
        oppekavaOis.setOasKinnitDoc(curriculum.getApprovalDokNr());
        oppekavaOis.setOasKinnitKp(date(curriculum.getApproval()));
        OisOppekeeled studyLang = new OisOppekeeled();
        for(CurriculumStudyLanguage csl : curriculum.getStudyLanguages()) {
            studyLang.getOppekeeleKood().add(value(csl.getStudyLang()));
        }
        oppekavaOis.setOppekeeled(studyLang);
        oppekavaOis.setNomKestusAasta(curriculum.getStudyPeriod() != null ? BigInteger.valueOf(curriculum.getStudyPeriod().longValue() / 12) : null);
        oppekavaOis.setNomKestusKuud(curriculum.getStudyPeriod() != null ? Integer.valueOf(curriculum.getStudyPeriod().intValue() % 12) : null);
        oppekavaOis.setOppekavaMaht(curriculum.getCredits() != null ? BigInteger.valueOf(curriculum.getCredits().longValue()) : null);
        oppekavaOis.setOppekavaRyhm(value(curriculum.getIscedClass()));
        Classifier group;
        if(higher) {
            group = curriculum.getGroup();
        } else if(curriculum.getIscedClass() != null) {
            group = ClassifierUtil.parentFor(curriculum.getIscedClass(), MainClassCode.OPPEKAVAGRUPP).orElse(null);
        } else {
            group = null;
        }
        oppekavaOis.setOppekavaGrupp(ehisValue(group));
        oppekavaOis.setAkadKraad(higher && curriculum.getGrades() != null && !curriculum.getGrades().isEmpty() ? ehisValue(curriculum.getGrades().stream().findFirst().get().getEhisGrade()) : null);
        BigDecimal practice;
        if(higher) {
            CurriculumVersion cv = curriculum.getVersions().stream().filter(CurriculumUtil::isCurriculumVersionConfirmed).findFirst().orElse(null);
            if(cv != null) {
                practice = cv.getModules().stream().flatMap(r -> r.getSubjects().stream()).map(r -> r.getSubject()).filter(r -> Boolean.TRUE.equals(r.getIsPractice()) && r.getCredits() != null).map(r -> r.getCredits()).reduce((x, y) -> x.add(y)).orElse(BigDecimal.ZERO);
            } else {
                practice = BigDecimal.ZERO;
            }
        } else {
            practice = curriculum.getModules().stream().filter(r -> Boolean.TRUE.equals(r.getPractice()) && r.getCredits() != null).map(r -> r.getCredits()).reduce((x, y) -> x.add(y)).orElse(BigDecimal.ZERO);
        }
        oppekavaOis.setPraktikaMaht(practice);
        
        OisOKSpetsialiseerumised specialities = new OisOKSpetsialiseerumised();
        Set<String> specialityNames = new HashSet<>();
        if (higher) {
            curriculum.getSpecialities().forEach(speciality -> {
                specialityNames.add(speciality.getNameEt());
            });
        } else {
            curriculum.getOccupations().stream()
                .flatMap(occupation -> occupation.getSpecialities().stream())
                .map(CurriculumOccupationSpeciality::getSpeciality).distinct()
                .forEach(speciality -> specialityNames.add(speciality.getNameEt()));
        }
        specialityNames.forEach(specialityName -> specialities.getOkSpetsialiseerumiseNimetus().add(specialityName));
        if (specialities.getOkSpetsialiseerumiseNimetus().isEmpty()) {
            specialities.getOkSpetsialiseerumiseNimetus().add(curriculum.getNameEt());
        }
        oppekavaOis.setSpetsialiseerumised(specialities);
        oppekavaOis.setVastavusRiikOppekava(!higher && curriculum.getStateCurriculum() != null ? value(curriculum.getStateCurriculum().getStateCurrClass()) : null);
        OisToimumiseKohad oisToimumiseKohad = new OisToimumiseKohad();
        for (CurriculumAddress address : curriculum.getAddresses()) {
            oisToimumiseKohad.getToimumiseKohtEHAK().add(
                    org.apache.commons.lang3.StringUtils.leftPad(address.getAddressOv(), 4, '0'));
        }
        oppekavaOis.setToimumiseKohad(oisToimumiseKohad);
        
        if(Boolean.TRUE.equals(curriculum.getJoint())) {
            for(CurriculumJointPartner cjp : curriculum.getJointPartners()) {
                OisYhisOppekava oisYhisOppekava = new OisYhisOppekava();
                if(cjp.isAbroad()) {
                    OisValisOAS oisValisOAS = new OisValisOAS();
                    oisValisOAS.setValisOASNimetus(cjp.getNameEt());
                    oisYhisOppekava.setValisOAS(oisValisOAS);
                } else {
                    oisYhisOppekava.setRegNumberEestiOAS(value2(cjp.getEhisSchool()));
                }
                oppekavaOis.getYhisOppekavaOas().add(oisYhisOppekava);
            }
        }
        
        OisKutsestandardid occupations = new OisKutsestandardid();
        if(curriculum.getOccupations().isEmpty()) {
            occupations.setPuudubKehtivKutsestandard(Integer.valueOf(1));
        } else {
            boolean occupationStandard = false;
            for(CurriculumOccupation co : curriculum.getOccupations()) {
                String occupationId = EntityUtil.getNullableCode(co.getOccupation());
                if(occupationId != null) {
                    Matcher m = OCCUPATION_ID_PREFIX.matcher(occupationId);
                    if(m.matches()) {
                        occupationStandard = true;
                        occupationId = value(co.getOccupation());
                        OisKutsestandard oisKutsestandard = new OisKutsestandard();
                        oisKutsestandard.setStandardReaId(new BigInteger(occupationId));
                        OiskutseSpetsialiseerumine oiskutseSpetsialiseerumine = new OiskutseSpetsialiseerumine();
                        for(CurriculumOccupationSpeciality cos : co.getSpecialities()) {
                            // FIXME validity check for classifier.value as number
                            oiskutseSpetsialiseerumine.getKutseSpetsialiseerumineReaId().add(new BigInteger(value(cos.getSpeciality())));
                        }
                        if (!oiskutseSpetsialiseerumine.getKutseSpetsialiseerumineReaId().isEmpty()) {
                            oisKutsestandard.setKutseSpetsialiseerumised(oiskutseSpetsialiseerumine);
                        }
                        occupations.getKutsestandard().add(oisKutsestandard);
                    }
                }
            }
            if(!occupationStandard && !higher) {
                // only partoccupations, determine occupations from partoccupations
                Map<String, OisKutsestandard> occMap = new HashMap<>();
                for(CurriculumOccupation cpo : curriculum.getOccupations()) {
                    String partOccupationId = EntityUtil.getNullableCode(cpo.getOccupation());
                    if(partOccupationId != null) {
                        Matcher pm = PARTOCCUPATION_ID_PREFIX.matcher(partOccupationId);
                        if(pm.matches()) {
                            partOccupationId = value(cpo.getOccupation());
                            OisOsakutse oisOsakutse = new OisOsakutse();
                            oisOsakutse.getOsakutseReaId().add(new BigInteger(partOccupationId));
                            Classifier occupation = ClassifierUtil.parentFor(cpo.getOccupation(), MainClassCode.KUTSE).orElse(null);
                            String occupationId = value(occupation);
                            if(occupationId == null) {
                                continue;
                            }
                            OisKutsestandard oisKutsestandard = occMap.get(occupationId);
                            if(oisKutsestandard == null) {
                                oisKutsestandard = new OisKutsestandard();
                                oisKutsestandard.setStandardReaId(new BigInteger(occupationId));
                                occupations.getKutsestandard().add(oisKutsestandard);
                                occMap.put(occupationId, oisKutsestandard);
                            }
                            oisKutsestandard.setOsakutsed(oisOsakutse);
                        }
                    }
                }
            }
        }
        oppekavaOis.setKutsestandardid(occupations);
        OisFailid oisFailid = new OisFailid();
        SoapAttachment attachment;
        try (ByteArrayOutputStream zipBytesOut = new ByteArrayOutputStream();
                ZipOutputStream zipOut = new ZipOutputStream(zipBytesOut)) {
            for(CurriculumFile cf : curriculum.getFiles()) {
                if(!cf.isSendEhis()) {
                    continue;
                }
                OisFail oisFail = new OisFail();
                oisFail.setTyyp(ehisValue(cf.getEhisFile()));
                OisFile oisFile = cf.getOisFile();
                oisFail.setNimi(oisFile.getFname());
                oisFail.setKommentaar(oisFile.getFdescription());
                oisFailid.getOkFail().add(oisFail);
                
                ZipEntry zipEntry = new ZipEntry(oisFile.getFname());
                zipOut.putNextEntry(zipEntry);
                byte[] bytes = oisFile.getFdata();
                zipOut.write(bytes, 0, bytes.length);
            }
            zipOut.close();
            zipBytesOut.close();
            attachment = new SoapAttachment(zipBytesOut.toByteArray(), "application/zip", ZIP_FILE_NAME);
        } catch (IOException e) {
            throw new HoisException("Failed to send curriculum files", e);
        }
        oisFailid.setContentID("cid:" + ZIP_FILE_NAME);
        oppekavaOis.setFailid(oisFailid);
        
        oppekavaOis.setKommentaar(curriculum.getDescription());
        oppekavad.getOppekava().add(oppekavaOis);

        EhisOisOppekavaResponse response = ehisClient.oisOppekava(getXroadHeader(), oisOppekava, attachment);
        logQuery(curriculum, response);

        if(response.hasError()) {
            throw new ValidationFailedException(getErrorMsg(higher, group, response.getLog()));
        }
        // check service result
        List<OisInfoteade> result = response.getResult();
        if(result != null && !result.isEmpty()) {
            OisInfoteade msg = response.getResult().get(0);
            if(BigInteger.ZERO.compareTo(msg.getVeakood()) != 0) {
                throw new ValidationFailedException(msg.getTeade());
            }
            // update merCode from EHIS
            BigInteger htmCode = msg.getOppekavaKood();
            if(htmCode != null) {
                curriculum.setMerCode(htmCode.toString());
            }
        }
    }
    
    private static String getErrorMsg(boolean higher, Classifier group, LogContext cxt) {
        if (cxt.getIncomingXml() != null) {
            if (!higher && group == null && Pattern.compile(".*<spring-ws:ValidationError.*>.*One of '\\{oppekavaGrupp\\}' is expected.</spring-ws:ValidationError>.*", Pattern.DOTALL)
                                        .matcher(cxt.getIncomingXml()).matches()) {
                return "curriculum.error.noConnectionBetweenRyhmAndGroup";
            }
        }
        
        return ExceptionUtil.getRootCause(cxt.getError()).toString();
    }

    public void updateFromEhis(HoisUserDetails userDetails, Curriculum curriculum, boolean isTest) {
        OisOppekavaStaatus oisOppekavaStaatus = new OisOppekavaStaatus();
        oisOppekavaStaatus.setRegNumber(value2(curriculum.getSchool().getEhisSchool()));
        Person person = em.getReference(Person.class, userDetails.getPersonId());
        oisOppekavaStaatus.setKasutajaIk(person.getIdcode());
        OisOppekavadStaatus oisOppekavadStaatus = new OisOppekavadStaatus();
        oisOppekavaStaatus.setOppekavad(oisOppekavadStaatus);

        OppekavaStaatusOis oppekavaStaatusOis = new OppekavaStaatusOis();
        // FIXME why here string? Other places have as BigInteger
        String curriculumCode = curriculum.getMerCode();
        BigInteger curriculumCodeNumber;
        try {
            curriculumCodeNumber = new BigInteger(curriculumCode);
        } catch(NumberFormatException | NullPointerException e) {
            String msgCode = e instanceof NullPointerException ? "curriculum.message.missingEhisCode" : "curriculum.message.badEhisCode";
            throw new ValidationFailedException(msgCode);
        }
        oppekavaStaatusOis.setOppekavaKood(curriculumCodeNumber);
        // XXX should create enum?
        oppekavaStaatusOis.setOperatsioon("KONTROLLIMINE");
        // TODO remove next row, for testing against mock server only
        oppekavaStaatusOis.setKommentaar(curriculum.getDescription());
        oisOppekavadStaatus.getOisOppekava().add(oppekavaStaatusOis);

        // TODO remove this block
        // EHIS oisOppekavadStaatus query is not working yet
        if(isTest) {
            // mark as confirmed
            curriculum.setStatus(em.getReference(Classifier.class, "OPPEKAVA_STAATUS_K"));
            curriculum.setEhisStatus(em.getReference(Classifier.class, "OPPEKAVA_EHIS_STAATUS_R"));
            return;
        }

        XRoadHeaderV4 header = getXroadHeader();
        header.getService().setServiceCode(OIS_OPPEKAVA_STAATUS_SERVICE_CODE);
        EhisOisOppekavaStaatusResponse response = ehisClient.oisOppekavaStaatus(header, oisOppekavaStaatus);
        logQuery(curriculum, response);

        if(response.hasError()) {
            throw new ValidationFailedException(response.getLog().getError().toString());
        }

        List<OisInfoteade> msgs = response.getResult();
        if(msgs != null && !msgs.isEmpty()) {
            OisInfoteade msg = msgs.stream().filter(r -> r.getOppekavaKood() != null && curriculumCodeNumber.compareTo(r.getOppekavaKood()) == 0).findFirst().orElse(null);
            if(msg != null) {
                if(BigInteger.ZERO.compareTo(msg.getVeakood()) != 0) {
                    throw new ValidationFailedException(msg.getTeade());
                }
                String ehisStatus = msg.getOppekavaStaatus();
                if(StringUtils.hasText(ehisStatus)) {
                    Classifier es = em.createQuery("select c from Classifier c where c.mainClassCode = ?1 and c.ehisValue= ?2", Classifier.class)
                            .setParameter(1, MainClassCode.OPPEKAVA_EHIS_STAATUS.name())
                            .setParameter(2, ehisStatus).setMaxResults(1).getResultList().stream().findAny().orElse(null);

                    if(es != null) {
                        curriculum.setEhisStatus(es);
                        curriculum.setEhisChanged(LocalDate.now());
                        // set curriculum status
                        String curriculumStatus = CURRICULUM_STATUS_FROM_EHIS_STATUS.get(EntityUtil.getCode(es));
                        if(curriculumStatus != null) {
                            curriculum.setStatus(em.getReference(Classifier.class, curriculumStatus));
                        }
                    }
                }
            }
        }
    }

    private <T extends LogResult<List<OisInfoteade>>> void logQuery(Curriculum curriculum, T response) {
        WsEhisCurriculumLog log = new WsEhisCurriculumLog();
        log.setSchool(curriculum.getSchool());
        log.setCurriculum(curriculum);
        if (response.hasError()) {
            log.setHasXteeErrors(Boolean.TRUE);
            log.setLogTxt(ExceptionUtil.getRootCause(response.getLog().getError()).toString());
        } else if (hasOtherErrors(response)) {
            log.setHasOtherErrors(Boolean.TRUE);
            log.setLogTxt(response.getResult().get(0).getTeade());
        }
        ehisLogService.insert(response.getLog(), log);
    }
    
    private static <T extends LogResult<List<OisInfoteade>>> boolean hasOtherErrors(T response) {
        if (response.getResult() != null && !response.getResult().isEmpty()) {
            return BigInteger.ZERO.compareTo(response.getResult().get(0).getVeakood()) != 0;
        }
        return false;
    }

    // TODO remove function - for mock test only
    /*
    @Override
    protected XRoadHeaderV4 getXroadHeader() {
        XRoadHeaderV4 header = super.getXroadHeader();
        header.setEndpoint("http://localhost:8087/services/ehis");
        return header;
    }*/

    @Override
    protected String getServiceCode() {
        return OIS_OPPEKAVA_SERVICE_CODE;
    }
}
