package ee.hitsa.ois.web;

import java.io.IOException;
import java.lang.invoke.MethodHandles;
import java.util.List;

import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;
import javax.validation.Valid;
import javax.validation.constraints.NotNull;

import ee.hitsa.ois.bdoc.MobileIdSigningSession;
import ee.hitsa.ois.domain.OisFile;
import ee.hitsa.ois.enums.Language;
import ee.hitsa.ois.web.dto.EntityMobileSignDto;
import org.digidoc4j.Container;
import org.digidoc4j.DataFile;
import org.digidoc4j.DataToSign;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import ee.hitsa.ois.bdoc.UnsignedBdocContainer;
import ee.hitsa.ois.domain.curriculum.CurriculumVersionOccupationModule;
import ee.hitsa.ois.domain.protocol.Protocol;
import ee.hitsa.ois.domain.protocol.ProtocolStudent;
import ee.hitsa.ois.report.ModuleProtocolReport;
import ee.hitsa.ois.service.BdocService;
import ee.hitsa.ois.service.ModuleProtocolService;
import ee.hitsa.ois.service.PdfService;
import ee.hitsa.ois.service.security.HoisUserDetails;
import ee.hitsa.ois.util.HttpUtil;
import ee.hitsa.ois.util.ModuleProtocolUtil;
import ee.hitsa.ois.util.UserUtil;
import ee.hitsa.ois.util.WithEntity;
import ee.hitsa.ois.util.WithVersionedEntity;
import ee.hitsa.ois.validation.Required;
import ee.hitsa.ois.validation.ValidationFailedException;
import ee.hitsa.ois.web.commandobject.ModuleProtocolCreateForm;
import ee.hitsa.ois.web.commandobject.ModuleProtocolSaveForm;
import ee.hitsa.ois.web.commandobject.ModuleProtocolSearchCommand;
import ee.hitsa.ois.web.commandobject.ModuleProtocolSignForm;
import ee.hitsa.ois.web.commandobject.ProtocolCalculateCommand;
import ee.hitsa.ois.web.commandobject.VersionedCommand;
import ee.hitsa.ois.web.commandobject.timetable.OtherStudentsSearchCommand;
import ee.hitsa.ois.web.dto.AutocompleteResult;
import ee.hitsa.ois.web.dto.EntitySignDto;
import ee.hitsa.ois.web.dto.ModuleProtocolDto;
import ee.hitsa.ois.web.dto.ModuleProtocolOccupationalModuleDto;
import ee.hitsa.ois.web.dto.ModuleProtocolSearchDto;
import ee.hitsa.ois.web.dto.ModuleProtocolStudentSelectDto;
import ee.hitsa.ois.web.dto.ProtocolStudentResultDto;


@RestController
@RequestMapping("/moduleProtocols")
public class ModuleProtocolController {

    private static final String BDOC_TO_SIGN = "moduleProtocolBdocContainerToSign";
    private static final String BDOC_CONT = "moduleProtocolBdocContainer";
    private static final String MOBILE_SESSIONID = "moduleProtocolBdocMobileSessionId";
    
    protected static final Logger log = LoggerFactory.getLogger(MethodHandles.lookup().lookupClass());

    @Autowired
    private ModuleProtocolService moduleProtocolService;
    @Autowired
    private BdocService bdocService;
    @Autowired
    private PdfService pdfService;

    @GetMapping
    public Page<ModuleProtocolSearchDto> search(HoisUserDetails user, @Valid ModuleProtocolSearchCommand command,
            Pageable pageable) {
        ModuleProtocolUtil.assertCanSearch(user);
        return moduleProtocolService.search(user, command, pageable);
    }

    @GetMapping("/{id:\\d+}")
    public ModuleProtocolDto get(HoisUserDetails user, @WithEntity Protocol protocol) {
        ModuleProtocolUtil.assertCanView(user, protocol);
        return moduleProtocolService.get(user, protocol);
    }

    /**
     * get() method was not used, because while building ModuleProtocolStudentDto, NPE appeared.
     * Solution: return only id after creation, as only id is required anyway for redirection to edit form.
     * Constructing DTO on edit form then goes without any problem. 
     */
    @PostMapping
    public ModuleProtocolDto create(HoisUserDetails user,
            @Valid @RequestBody ModuleProtocolCreateForm moduleProtocolCreateForm) {
        ModuleProtocolUtil.assertCanCreate(user);
        return ModuleProtocolDto.onlyId(moduleProtocolService.create(user, moduleProtocolCreateForm));
    }

    @PutMapping("/{id:\\d+}")
    public ModuleProtocolDto save(HoisUserDetails user,
            @WithVersionedEntity(versionRequestBody = true) Protocol protocol,
            @Valid @RequestBody ModuleProtocolSaveForm moduleProtocolSaveForm) {
        ModuleProtocolUtil.assertCanEdit(user, protocol);
        return get(user, moduleProtocolService.save(protocol, moduleProtocolSaveForm));
    }

    @DeleteMapping("/{id:\\d+}")
    public void delete(HoisUserDetails user,
            @WithVersionedEntity(versionRequestParam = "version") Protocol protocol,
            @SuppressWarnings("unused") @RequestParam("version") Long version) {
        ModuleProtocolUtil.assertCanDelete(user, protocol);
        moduleProtocolService.delete(user, protocol);
    }

    @GetMapping("occupationModules/{curriculumVersionId:\\d+}")
    public List<AutocompleteResult> occupationModules(HoisUserDetails user, @PathVariable Long curriculumVersionId) {
        ModuleProtocolUtil.assertCanCreate(user);
        return moduleProtocolService.occupationModules(user, curriculumVersionId);
    }

    @GetMapping("occupationModule/{studyYearId:\\d+}/{curriculumVersionOccupationModuleId:\\d+}")
    public ModuleProtocolOccupationalModuleDto occupationModule(HoisUserDetails user, @PathVariable Long studyYearId,
            @PathVariable Long curriculumVersionOccupationModuleId) {
        ModuleProtocolUtil.assertCanCreate(user);
        return moduleProtocolService.occupationModule(user, studyYearId, curriculumVersionOccupationModuleId);
    }

    @GetMapping("/{id:\\d+}/otherStudents")
    public Page<ModuleProtocolStudentSelectDto> otherStudents(HoisUserDetails user, @WithEntity Protocol protocol,
            OtherStudentsSearchCommand command, Pageable pageable) {
        ModuleProtocolUtil.assertCanCreate(user);
        return moduleProtocolService.otherStudents(user, protocol, command, pageable);
    }

    @PostMapping("/{id:\\d+}/addStudents")
    public ModuleProtocolDto addStudents(HoisUserDetails user,
            @WithVersionedEntity(versionRequestBody = true) Protocol protocol,
            @Valid @RequestBody ModuleProtocolSaveForm moduleProtocolSaveForm) {
        ModuleProtocolUtil.assertCanEdit(user, protocol);
        return get(user, moduleProtocolService.addStudents(protocol, moduleProtocolSaveForm));
    }
    
    @DeleteMapping("/{id:\\d+}/removeStudent/{studentId:\\d+}")
    public ModuleProtocolDto removeStudent(HoisUserDetails user, @WithEntity Protocol protocol,
            @WithEntity("studentId") ProtocolStudent student) {
        ModuleProtocolUtil.assertCanEdit(user, protocol);
        moduleProtocolService.removeStudent(user, student);
        return get(user, protocol);
    }

    @PostMapping("/{id:\\d+}/signToConfirm")
    public EntitySignDto signToConfirm(HoisUserDetails user,
            @WithVersionedEntity(versionRequestBody = true) Protocol protocol,
            @Valid @RequestBody ModuleProtocolSignForm moduleProtocolSignForm, HttpSession httpSession) {
        ModuleProtocolUtil.assertCanConfirm(user, protocol);
        Protocol savedProtocol = moduleProtocolService.save(protocol, moduleProtocolSignForm);
        UnsignedBdocContainer unsignedBdocContainer = bdocService.createUnsignedBdocContainer("mooduli_protokoll.pdf", 
                MediaType.APPLICATION_PDF_VALUE, pdfService.generate(ModuleProtocolReport.TEMPLATE_NAME,
                        moduleProtocolService.moduleProtocolReport(protocol)), 
                moduleProtocolSignForm.getCertificate());

        httpSession.setAttribute(BDOC_TO_SIGN, unsignedBdocContainer.getDataToSign());
        httpSession.setAttribute(BDOC_CONT, unsignedBdocContainer.getContainer());
        
        return EntitySignDto.of(savedProtocol, unsignedBdocContainer);
    }

    @PostMapping("/{id:\\d+}/signToConfirmFinalize")
    public ModuleProtocolDto signToConfirmFinalize(HoisUserDetails user, @WithVersionedEntity(versionRequestBody = true) Protocol protocol,
            @Valid @RequestBody SignatureCommand signatureCommand, HttpSession httpSession) {
        UserUtil.assertIsSchoolAdminOrTeacher(user, protocol.getSchool());

        DataToSign dataToSign = (DataToSign) httpSession.getAttribute(BDOC_TO_SIGN);
        Container container = (Container) httpSession.getAttribute(BDOC_CONT);

        protocol.setOisFile(bdocService.getSignedBdoc(container, dataToSign, signatureCommand.getSignature(), "protokoll"));

        httpSession.removeAttribute(BDOC_TO_SIGN);
        httpSession.removeAttribute(BDOC_CONT);
        return get(user, moduleProtocolService.confirm(user, protocol, null));
    }


    @PostMapping("/{id:\\d+}/mobileIdSignatureRequest")
    public EntityMobileSignDto mobileIdSignatureRequest(HoisUserDetails user,
            @WithVersionedEntity(versionRequestBody = true) Protocol protocol,
            @Valid @RequestBody ModuleProtocolSaveForm moduleProtocolSaveForm,
            HttpSession httpSession, Language lang) {
        ModuleProtocolUtil.assertCanConfirm(user, protocol);

        Protocol savedProtocol = moduleProtocolService.save(protocol, moduleProtocolSaveForm);
        byte[] pdfData = pdfService.generate(ModuleProtocolReport.TEMPLATE_NAME,
                moduleProtocolService.moduleProtocolReport(savedProtocol));

        DataFile dataFile = new DataFile(pdfData, "mooduli_protokoll.pdf", MediaType.APPLICATION_PDF_VALUE);
        String mobileNumber = UserUtil.isOAuthLoginType(user) ? moduleProtocolSaveForm.getSignerMobileNumber()
                : user.getMobileNumber();
        MobileIdSigningSession session = bdocService.mobileIdSignatureRequest(user.getIdcode(), mobileNumber,
                dataFile, lang);
        httpSession.setAttribute(MOBILE_SESSIONID, session.getSessionID());
        httpSession.setAttribute(BDOC_TO_SIGN, session.getDataToSign());
        httpSession.setAttribute(BDOC_CONT, session.getContainer());
        return EntityMobileSignDto.of(protocol, session.getVerificationCode());
    }

    @PostMapping("/{id:\\d+}/mobileIdSign")
    public ModuleProtocolDto mobileIdSign(HoisUserDetails user,
            @WithVersionedEntity(versionRequestBody = true) Protocol protocol,
            @SuppressWarnings("unused") @RequestBody VersionedCommand version, HttpSession httpSession) {
        String sessionID = (String) httpSession.getAttribute(MOBILE_SESSIONID);
        DataToSign dataToSign = (DataToSign) httpSession.getAttribute(BDOC_TO_SIGN);
        Container container = (Container) httpSession.getAttribute(BDOC_CONT);

        if (sessionID != null) {
            MobileIdSigningSession session = new MobileIdSigningSession(sessionID, dataToSign, container);
            OisFile signedBdoc = bdocService.mobileIdSign(session, "protokoll");
            if (signedBdoc != null) {
                protocol.setOisFile(signedBdoc);
                protocol = moduleProtocolService.confirm(user, protocol, null);
            }
            httpSession.removeAttribute(MOBILE_SESSIONID);
            httpSession.removeAttribute(BDOC_TO_SIGN);
            httpSession.removeAttribute(BDOC_CONT);
        }
        return get(user, protocol);
    }

    @GetMapping("/{id:\\d+}/print/protocol.pdf")
    public void print(HoisUserDetails user, @WithEntity Protocol protocol, HttpServletResponse response)
            throws IOException {
        ModuleProtocolUtil.assertCanView(user, protocol);
        HttpUtil.pdf(response, protocol.getProtocolNr() + ".pdf", pdfService
                .generate(ModuleProtocolReport.TEMPLATE_NAME, moduleProtocolService.moduleProtocolReport(protocol)));
    }

    @GetMapping("/{id:\\d+}/calculate")
    public List<ProtocolStudentResultDto> calculateGrades(HoisUserDetails user,
            @NotNull @Valid ProtocolCalculateCommand command, @WithEntity Protocol protocol) {
        ModuleProtocolUtil.assertCanEdit(user, protocol);
        if(!Boolean.TRUE.equals(protocol.getIsVocational())) {
            throw new ValidationFailedException("not vocational protocol");
        }
        return moduleProtocolService.calculateGrades(command);
    }

    @GetMapping("/myModules")
    public Page<ModuleProtocolService.TeacherModuleMinimumDto> myModules(HoisUserDetails user,
            @RequestParam(required = true) Long studyYear, Pageable pageable) {
        UserUtil.assertIsTeacher(user);
        return moduleProtocolService.availableTeacherModules(user, studyYear, pageable);
    }

    @GetMapping("/moduleHistory/{id:\\d+}")
    public ModuleProtocolService.LessonPlanHistoryDto getModuleLessonPlanHistory(HoisUserDetails user,
            @WithEntity CurriculumVersionOccupationModule module) {
        UserUtil.assertIsTeacher(user);
        return moduleProtocolService.getModuleLessonPlanHistory(module);
    }
}

class SignatureCommand extends VersionedCommand {

    @Required
    private String signature;

    public String getSignature() {
        return signature;
    }

    public void setSignature(String signature) {
        this.signature = signature;
    }
}
