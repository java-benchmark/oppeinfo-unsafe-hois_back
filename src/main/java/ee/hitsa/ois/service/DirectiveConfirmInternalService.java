package ee.hitsa.ois.service;

import ee.hitsa.ois.domain.directive.Directive;
import ee.hitsa.ois.domain.school.School;
import ee.hitsa.ois.enums.*;
import ee.hitsa.ois.exception.HoisException;
import ee.hitsa.ois.service.fotobox.FotoBoxService;
import ee.hitsa.ois.util.*;
import ee.hitsa.ois.web.commandobject.directive.DirectiveConfirmForm;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.scheduling.TaskScheduler;
import org.springframework.stereotype.Service;

import javax.persistence.EntityManager;
import javax.persistence.EntityNotFoundException;
import javax.transaction.Transactional;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.util.Date;

@Service
public class DirectiveConfirmInternalService {

    @Autowired
    private EntityManager em;
    @Autowired
    private DirectiveConfirmService directiveConfirmService;
    @Autowired
    private FotoBoxService fotoBoxService;
    @Autowired
    private TaskScheduler scheduler;

    private Directive confirm(String confirmer, Directive directive, LocalDate confirmDate, boolean isEKIS) {
        directiveConfirmService.confirm(confirmer, directive, confirmDate);
        if (ClassifierUtil.oneOf(directive.getType(), DirectiveType.KASKKIRI_IMMAT, DirectiveType.KASKKIRI_IMMATV, 
                DirectiveType.KASKKIRI_EKSTERN, DirectiveType.KASKKIRI_KYLALIS, DirectiveType.KASKKIRI_ENNIST)) {
//            fotoBoxService.directiveStudentPhotoAsyncRequest(confirmer, directive);
            scheduler.schedule(() -> fotoBoxService.directiveStudentPhotoJobRequest(confirmer, directive, isEKIS),
                    Date.from(LocalDateTime.now().plusSeconds(30).atZone(ZoneId.systemDefault()).toInstant()));
        }
        return directive;
    }

    @Transactional(Transactional.TxType.REQUIRES_NEW)
    public Directive confirmedByEkis(long directiveId, String directiveNr, LocalDate confirmDate, String preamble, long wdId,
                                     String signerIdCode, String signerName, long schoolId) {
        //TODO veahaldus
        //• ÕISi käskkiri ei leitud – ÕISist ei leita vastava vastava OIS_ID ja WD_ID-ga käskkirja
        //• Vale staatus – kinnitada saab ainult „kinnitamisel“ staatusega käskkirja. „Koostamisel“ ja „Kinnitatud“ käskkirju kinnitada ei saa.
        //• Üldine veateade – üldine viga salvestamisel, nt liiga lühike andmeväli, vale andmetüüp vms
        Directive directive = findDirective(directiveId, wdId, schoolId);
        directive.setDirectiveNr(directiveNr);
        directive.setPreamble(preamble);
        return confirm(PersonUtil.fullnameAndIdcode(signerName, signerIdCode), directive, confirmDate, true);
    }

    @Transactional(Transactional.TxType.NEVER)
    public Directive confirmedByUser(String confirmer, Directive directive, LocalDate confirmDate, DirectiveConfirmForm form) {
        directive.setDirectiveNr(form.getDirectiveNr());
        directive.setPreamble(form.getPreamble());
        return confirm(confirmer, directive, confirmDate, false);
    }

    private Directive findDirective(long directiveId, long wdId, long schoolId) {
        try {
            Directive directive = em.getReference(Directive.class, Long.valueOf(directiveId));

            School school = directive.getSchool();
            // ekis true, school 0 - throw
            // ekis false, school 0 - OK
            // ekis true, school !0 - OK, if same school
            // ekis false, school !0 - throw

            if (
                    (school.getEkisUrl() != null && (Long.valueOf(0).equals(schoolId) || !school.getId().equals(schoolId)))
                ||
                    (school.getEkisUrl() == null && !Long.valueOf(0).equals(schoolId))
            ) {
                throw new EntityNotFoundException();
            }

            if (!ClassifierUtil.equals(DirectiveStatus.KASKKIRI_STAATUS_KINNITAMISEL, directive.getStatus())) {
                throw new HoisException("Käskkiri vale staatusega");
            }
            if (directive.getWdId() == null || directive.getWdId().longValue() != wdId) {
                throw new HoisException("Käskkiri vale ekise id-ga");
            }
            return directive;
        } catch (@SuppressWarnings("unused") EntityNotFoundException e) {
            throw new HoisException("Käskkirja ei leitud");
        }
    }
}
