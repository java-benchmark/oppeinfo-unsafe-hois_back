package ee.hitsa.ois.web;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import javax.persistence.EntityManager;
import javax.servlet.http.HttpServletResponse;
import javax.validation.Valid;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import ee.hitsa.ois.domain.Contract;
import ee.hitsa.ois.domain.ContractSupervisor;
import ee.hitsa.ois.domain.school.School;
import ee.hitsa.ois.enums.ContractStatus;
import ee.hitsa.ois.enums.Language;
import ee.hitsa.ois.enums.Permission;
import ee.hitsa.ois.enums.PermissionObject;
import ee.hitsa.ois.report.PracticeContractReport;
import ee.hitsa.ois.service.ContractService;
import ee.hitsa.ois.service.RtfService;
import ee.hitsa.ois.service.security.HoisUserDetails;
import ee.hitsa.ois.util.ClassifierUtil;
import ee.hitsa.ois.util.EntityUtil;
import ee.hitsa.ois.util.HttpUtil;
import ee.hitsa.ois.util.PersonUtil;
import ee.hitsa.ois.util.UserUtil;
import ee.hitsa.ois.util.WithEntity;
import ee.hitsa.ois.util.WithVersionedEntity;
import ee.hitsa.ois.validation.ValidationFailedException;
import ee.hitsa.ois.web.commandobject.ContractAllCommand;
import ee.hitsa.ois.web.commandobject.ContractCancelForm;
import ee.hitsa.ois.web.commandobject.ContractEkisForm;
import ee.hitsa.ois.web.commandobject.ContractForEkisSearchCommand;
import ee.hitsa.ois.web.commandobject.ContractForm;
import ee.hitsa.ois.web.commandobject.ContractSearchCommand;
import ee.hitsa.ois.web.dto.AutocompleteResult;
import ee.hitsa.ois.web.dto.ContractDto;
import ee.hitsa.ois.web.dto.ContractEkisDto;
import ee.hitsa.ois.web.dto.ContractForEkisDto;
import ee.hitsa.ois.web.dto.ContractNrCommand;
import ee.hitsa.ois.web.dto.ContractSearchDto;
import ee.hitsa.ois.web.dto.ContractStudentModuleDto;
import ee.hitsa.ois.web.dto.ContractStudentSubjectDto;
import ee.hitsa.ois.web.dto.ContractToEkisMessageDto;
import ee.hitsa.ois.web.dto.StudentGroupContractSearchCommand;
import ee.hitsa.ois.web.dto.StudentGroupContractSearchDto;

@RestController
@RequestMapping("/contracts")
public class ContractController {

    @Autowired
    private ContractService contractService;
    @Autowired
    private RtfService rtfService;

    @Autowired
    private EntityManager em;

    @GetMapping
    public Page<ContractSearchDto> search(HoisUserDetails user, ContractSearchCommand command, Pageable pageable) {
        assertCanSearch(user);
        return contractService.search(user, command, pageable);
    }

    @GetMapping("/all")
    public Page<ContractSearchDto> search(HoisUserDetails user, ContractAllCommand command, Pageable pageable) {
        UserUtil.assertIsSchoolAdminOrLeadingTeacher(user, Permission.OIGUS_M, PermissionObject.TEEMAOIGUS_LEPING);
        return contractService.searchAll(user, command, pageable);
    }

    @GetMapping("/studentGroup")
    public Page<StudentGroupContractSearchDto> searchStudentGroupContract(HoisUserDetails user,
            @Valid StudentGroupContractSearchCommand command, Pageable pageable) {
        UserUtil.assertIsSchoolAdminOrLeadingTeacher(user);
        return contractService.searchStudentGroupContract(user, command, pageable);
    }

    @GetMapping("/ekis")
    public Page<ContractForEkisDto> searchContractForEkis(HoisUserDetails user,
            @Valid ContractForEkisSearchCommand command, Pageable pageable) {
        UserUtil.assertIsSchoolAdminOrLeadingTeacher(user);
        return contractService.searchContractForEkis(user, command, pageable);
    }

    @GetMapping("/{id:\\d+}")
    public ContractDto get(HoisUserDetails user, @WithEntity Contract contract) {
        assertCanView(user, contract);
        return contractService.get(user, contract);
    }

    @PostMapping
    public ContractDto create(HoisUserDetails user, @Valid @RequestBody ContractForm contractForm) {
        UserUtil.assertIsSchoolAdminOrLeadingTeacher(user, Permission.OIGUS_M, PermissionObject.TEEMAOIGUS_LEPING);
        Contract savedContract = null;
        if (contractForm.getStudents() != null && !contractForm.getStudents().isEmpty()) {
            for (AutocompleteResult student : contractForm.getStudents()) {
               ContractForm copyOfContracForm = contractForm;
               copyOfContracForm.setStudent(student);
               savedContract = contractService.create(user, contractForm);
            }
            if (savedContract != null) {
                return get(user, savedContract);
            }
        } else {
            savedContract = contractService.create(user, contractForm);
        }
        return get(user, savedContract);
    }

    @PutMapping("/{id:\\d+}")
    public ContractDto save(HoisUserDetails user,
            @WithVersionedEntity(versionRequestBody = true) Contract contract,
            @Valid @RequestBody ContractForm contractForm) {
        UserUtil.assertIsSchoolAdminOrLeadingTeacher(user, contract.getStudent().getSchool(), Permission.OIGUS_M,
                PermissionObject.TEEMAOIGUS_LEPING);
        if (!ClassifierUtil.equals(ContractStatus.LEPING_STAATUS_S, contract.getStatus())) {
            throw new ValidationFailedException("contract.messages.updatingOnlyAllowedForStatusS");
        }
        return get(user, contractService.save(user, contract, contractForm));
    }
    
    @PutMapping("/cancel/{id:\\d+}")
    public ContractDto cancel(HoisUserDetails user,
            @WithVersionedEntity(versionRequestBody = true) Contract contract,
            @Valid @RequestBody ContractCancelForm contractForm) {
        UserUtil.assertIsSchoolAdminOrLeadingTeacher(user, contract.getStudent().getSchool(), Permission.OIGUS_M,
                PermissionObject.TEEMAOIGUS_LEPING);
        if (!ClassifierUtil.equals(ContractStatus.LEPING_STAATUS_K, contract.getStatus()) && !ClassifierUtil.equals(ContractStatus.LEPING_STAATUS_Y, contract.getStatus())) {
            throw new ValidationFailedException("contract.messages.updatingOnlyAllowedForStatusKandY");
        }
        return get(user, contractService.cancel(user, contract, contractForm));
    }

    @DeleteMapping("/{id:\\d+}")
    public void delete(HoisUserDetails user,
            @WithVersionedEntity(versionRequestParam = "version") Contract contract,
            @SuppressWarnings("unused") @RequestParam("version") Long version) {
        UserUtil.assertIsSchoolAdminOrLeadingTeacher(user, contract.getStudent().getSchool(), Permission.OIGUS_M,
                PermissionObject.TEEMAOIGUS_LEPING);
        if (!ClassifierUtil.equals(ContractStatus.LEPING_STAATUS_Y, contract.getStatus())
                && !ClassifierUtil.equals(ContractStatus.LEPING_STAATUS_S, contract.getStatus())) {
            throw new ValidationFailedException("contract.messages.deletionOnlyAllowedForStatusSAndY");
        }
        contractService.delete(user, contract);
    }

    @GetMapping("studentPracticeModules/{studentId:\\d+}")
    public Collection<ContractStudentModuleDto> studentPracticeModules(HoisUserDetails user,
            @PathVariable Long studentId) {
        UserUtil.assertIsSchoolAdminOrLeadingTeacher(user, Permission.OIGUS_M, PermissionObject.TEEMAOIGUS_LEPING);
        return contractService.studentPracticeModules(user, studentId);
    }

    @GetMapping("studentPracticeSubjects/{studentId:\\d+}")
    public Collection<ContractStudentSubjectDto> studentSubjects(HoisUserDetails user, @PathVariable Long studentId) {
        UserUtil.assertIsSchoolAdminOrLeadingTeacher(user, Permission.OIGUS_M, PermissionObject.TEEMAOIGUS_LEPING);
        return contractService.studentPracticeHigherModules(user, studentId)
                .stream().flatMap(it -> it.getSubjects().stream()).collect(Collectors.toList());
    }

    @GetMapping("/checkForEkis/{id:\\d+}")
    public Map<String, ?> checkForEkis(HoisUserDetails user, @PathVariable("id") Long contractId) {
        UserUtil.assertIsSchoolAdminOrLeadingTeacher(user, Permission.OIGUS_V, PermissionObject.TEEMAOIGUS_LEPING);
        return contractService.checkForEkis(user, contractId);
    }

    @GetMapping("/checkForEkis")
    public Map<String, ?> checkForEkis(HoisUserDetails user) {
        UserUtil.assertIsSchoolAdminOrLeadingTeacher(user);
        return contractService.checkForEkis(user, null);
    }

    @PutMapping("/sendEmail/{id:\\d+}")
    public void sendEmail(HoisUserDetails user, @WithEntity ContractSupervisor supervisor) {
        UserUtil.assertIsSchoolAdminOrLeadingTeacher(user, supervisor.getContract().getStudent(), Permission.OIGUS_M,
                PermissionObject.TEEMAOIGUS_LEPING);
        contractService.sendUniqueUrlEmailToEnterpriseSupervisor(user, supervisor);
    }

    @PostMapping("/checkout/{id:\\d+}")
    public ContractDto checkout(HoisUserDetails user, @WithEntity Contract contract) {
        UserUtil.assertIsSchoolAdminOrLeadingTeacher(user, contract.getStudent(), Permission.OIGUS_M,
                PermissionObject.TEEMAOIGUS_LEPING);
        UserUtil.assertIsSchoolWithoutEkis(em.getReference(School.class, user.getSchoolId()));
        return get(user, contractService.checkout(user, contract));
    }
    
    @PostMapping("/confirm/{id:\\d+}")
    public ContractDto confirm(HoisUserDetails user, @WithEntity Contract contract) {
        UserUtil.assertIsSchoolAdminOrLeadingTeacher(user, contract.getStudent(), Permission.OIGUS_M,
                PermissionObject.TEEMAOIGUS_LEPING);
        UserUtil.assertIsSchoolWithoutEkis(em.getReference(School.class, user.getSchoolId()));
        return get(user, contractService.confirm(contract));
    }

    @PostMapping("/sendToEkis/{id:\\d+}")
    public ContractDto sendToEkis(HoisUserDetails user, @WithEntity Contract contract) {
        UserUtil.assertIsSchoolAdminOrLeadingTeacher(user, contract.getStudent(), Permission.OIGUS_M,
                PermissionObject.TEEMAOIGUS_LEPING);
        return get(user, contractService.sendToEkis(user, contract));
    }
    
    @PostMapping("/sendToEkis")
    public ContractEkisDto sendToEkis(HoisUserDetails user, @RequestBody ContractEkisForm contractEkisForm) {
        UserUtil.assertIsSchoolAdminOrLeadingTeacher(user, Permission.OIGUS_M, PermissionObject.TEEMAOIGUS_LEPING);
        List<ContractToEkisMessageDto> passed = new ArrayList<>();
        List<ContractToEkisMessageDto> failed = new ArrayList<>();
        for (Long id : contractEkisForm.getContracts()) {
            Contract contract = em.getReference(Contract.class, id);
            try {
                contractService.sendToEkis(user, contract);
                passed.add(ContractService.createContractToEkisDto(contract, "contract.messages.sendToEkis.success"));
            } catch(Exception e) {
                failed.add(ContractService.createContractToEkisDto(contract, e.getMessage()));
            }
        }
        ContractEkisDto dto = new ContractEkisDto();
        dto.setFailed(failed);
        dto.setSuccessful(passed);
        return dto;
    }

    @GetMapping("/print/{id:\\d+}/contract.rtf")
    public void printRtf(HoisUserDetails user, @WithEntity Contract contract, HttpServletResponse response, @RequestParam(required = false) Language lang) throws IOException {
        assertCanView(user, contract);
        HttpUtil.rtf(response, String.format("praktikaleping_%s.rtf", PersonUtil.fullname(contract.getStudent().getPerson())),
                rtfService.generateFop(PracticeContractReport.TEMPLATE_NAME, new PracticeContractReport(contract, lang), lang));
    }

    @PutMapping("/changeContractNr/{id:\\d+}")
    public ContractDto changeContractNr(HoisUserDetails user, @WithEntity Contract contract, @RequestBody ContractNrCommand command) {
        UserUtil.assertIsSchoolAdminOrLeadingTeacher(user, contract.getStudent(), Permission.OIGUS_M,
                PermissionObject.TEEMAOIGUS_LEPING);
        UserUtil.assertIsSchoolWithoutEkis(em.getReference(School.class, user.getSchoolId()));
        return get(user, contractService.changeContractNr(contract, command));
        
    }

    private static void assertCanSearch(HoisUserDetails user) {
        if (user.isSchoolAdmin() || user.isLeadingTeacher()) {
            UserUtil.throwAccessDeniedIf(
                    !UserUtil.hasPermission(user, Permission.OIGUS_V, PermissionObject.TEEMAOIGUS_LEPING));
        } else {
            UserUtil.assertIsStudent(user);
        }
    }

    /**
     * Used in {@link #get(HoisUserDetails, Contract)} and {@link #printRtf(HoisUserDetails, Contract, HttpServletResponse, Language)}
     * 
     * @param user
     * @param contract
     * @return
     */
    private static void assertCanView(HoisUserDetails user, Contract contract) {
        if (user.isTeacher()) {
            UserUtil.throwAccessDeniedIf(!EntityUtil.getId(contract.getTeacher()).equals(user.getTeacherId()));
        } else if (user.isStudent() || user.isRepresentative()) {
            UserUtil.throwAccessDeniedIf(!EntityUtil.getId(contract.getStudent()).equals(user.getStudentId()));
        } else {
            UserUtil.assertIsSchoolAdminOrLeadingTeacher(user, contract.getStudent(), Permission.OIGUS_V,
                    PermissionObject.TEEMAOIGUS_LEPING);
        }
    }
}
