package ee.hitsa.ois.web;

import java.util.Set;

import javax.validation.Valid;
import javax.validation.constraints.NotNull;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import ee.hitsa.ois.domain.Committee;
import ee.hitsa.ois.service.CommitteeService;
import ee.hitsa.ois.service.security.HoisUserDetails;
import ee.hitsa.ois.util.CommitteeUserRights;
import ee.hitsa.ois.util.UserUtil;
import ee.hitsa.ois.util.WithEntity;
import ee.hitsa.ois.util.WithVersionedEntity;
import ee.hitsa.ois.web.commandobject.CommitteeSearchCommand;
import ee.hitsa.ois.web.dto.AutocompleteResult;
import ee.hitsa.ois.web.dto.CommitteeDto;
import ee.hitsa.ois.web.dto.CommitteeSearchDto;

@RestController
@RequestMapping("/committees")
public class CommitteeController {

    @Autowired
    private CommitteeService committeeService;

    @GetMapping
    public Page<CommitteeSearchDto> search(HoisUserDetails user, @NotNull CommitteeSearchCommand criteria,
            Pageable pageable) {
        UserUtil.throwAccessDeniedIf(!CommitteeUserRights.canSearch(user));
        return committeeService.search(user.getSchoolId(), criteria, pageable);
    }

    @GetMapping("/{id:\\d+}")
    public CommitteeDto get(HoisUserDetails user, @WithEntity Committee committee) {
        UserUtil.throwAccessDeniedIf(!CommitteeUserRights.canView(user, committee));
        return committeeService.get(user, committee);
    }

    @PostMapping
    public CommitteeDto create(HoisUserDetails user, @NotNull @Valid @RequestBody CommitteeDto dto) {
        UserUtil.throwAccessDeniedIf(!CommitteeUserRights.canCreate(user));
        return get(user, committeeService.create(user.getSchoolId(), dto));
    }

    @PutMapping("/{id:\\d+}")
    public CommitteeDto save(HoisUserDetails user, @WithVersionedEntity(versionRequestBody = true) Committee committee,
            @NotNull @Valid @RequestBody CommitteeDto dto) {
        UserUtil.throwAccessDeniedIf(!CommitteeUserRights.canEdit(user, committee), "User cannot edit committee");
        return get(user, committeeService.save(committee, dto));
    }

    @DeleteMapping("/{id:\\d+}")
    public void delete(HoisUserDetails user, @WithVersionedEntity(versionRequestParam = "version") Committee committee,
            @SuppressWarnings("unused") @RequestParam("version") Long version) {
        UserUtil.throwAccessDeniedIf(!CommitteeUserRights.canDelete(user, committee), "User cannot edit committee");
        committeeService.delete(user, committee);
    }

    @GetMapping("/members")
    public Set<AutocompleteResult> getMembers(HoisUserDetails user) {
        UserUtil.assertIsSchoolAdminOrLeadingTeacherOrTeacher(user);
        return committeeService.getMembers(user.getSchoolId());
    }
}
