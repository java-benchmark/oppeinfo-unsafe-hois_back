package ee.hitsa.ois.web.dto.application;

import java.util.Set;

import javax.validation.constraints.Size;

import ee.hitsa.ois.domain.application.ApplicationPlannedSubject;
import ee.hitsa.ois.util.EntityUtil;
import ee.hitsa.ois.util.StreamUtil;
import ee.hitsa.ois.web.commandobject.VersionedCommand;

public class ApplicationPlannedSubjectDto extends VersionedCommand {

    private Long id;

    @Size(max = 1000)
    private String name;

    private Set<ApplicationPlannedSubjectEquivalentDto> equivalents;

    public static ApplicationPlannedSubjectDto of(ApplicationPlannedSubject plannedSubject) {
        ApplicationPlannedSubjectDto dto = EntityUtil.bindToDto(plannedSubject, new ApplicationPlannedSubjectDto(), "equivalents");
        dto.setEquivalents(StreamUtil.toMappedSet(ApplicationPlannedSubjectEquivalentDto::of, plannedSubject.getEquivalents()));

        return dto;
    }

    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public Set<ApplicationPlannedSubjectEquivalentDto> getEquivalents() {
        return equivalents;
    }

    public void setEquivalents(Set<ApplicationPlannedSubjectEquivalentDto> equivalents) {
        this.equivalents = equivalents;
    }

}
