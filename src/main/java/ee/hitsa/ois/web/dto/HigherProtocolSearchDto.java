package ee.hitsa.ois.web.dto;

import java.time.LocalDate;
import java.util.Collections;
import java.util.List;

import ee.hitsa.ois.domain.protocol.Protocol;
import ee.hitsa.ois.domain.subject.studyperiod.SubjectStudyPeriod;
import ee.hitsa.ois.service.security.HoisUserDetails;
import ee.hitsa.ois.util.EntityUtil;
import ee.hitsa.ois.util.HigherProtocolUtil;
import ee.hitsa.ois.util.PersonUtil;

public class HigherProtocolSearchDto {

    private Long id;
    private String protocolNr;
    private String protocolType;
    private LocalDate protocolDate;
    private String status;
    private AutocompleteResult subject;
    private AutocompleteResult module;
    private List<String> teachers;
    private List<String> studentGroups;
    private LocalDate inserted;
    private LocalDate confirmDate;
    private String confirmer;
    private Boolean canChange;
    
    public static HigherProtocolSearchDto ofWithUserRights(Protocol protocol, HoisUserDetails user) {
        HigherProtocolSearchDto dto = new HigherProtocolSearchDto();
        EntityUtil.bindToDto(protocol, dto, "confirmer");
        dto.setProtocolType(EntityUtil.getCode(protocol.getProtocolHdata().getType()));
        dto.setProtocolDate(protocol.getFinalDate());
        dto.setInserted(protocol.getInserted().toLocalDate());

        SubjectStudyPeriod subjectStudyPeriod = protocol.getProtocolHdata().getSubjectStudyPeriod();
        if (subjectStudyPeriod != null) {
            dto.setSubject(AutocompleteResult.of(subjectStudyPeriod.getSubject()));
            dto.setTeachers(PersonUtil.sorted(subjectStudyPeriod.getTeachers().stream().map(t -> t.getTeacher().getPerson())));
        } else {
            dto.setModule(AutocompleteResult.of(protocol.getProtocolHdata().getCurriculumVersionHmodule()));
            dto.setTeachers(Collections.singletonList(PersonUtil.fullname(protocol.getProtocolHdata().getTeacher().getPerson())));
        }

        dto.setCanChange(Boolean.valueOf(HigherProtocolUtil.canChange(user, protocol)));
        dto.setConfirmer(PersonUtil.stripIdcodeFromFullnameAndIdcode(protocol.getConfirmer()));
        return dto;
    }

    public Boolean getCanChange() {
        return canChange;
    }

    public void setCanChange(Boolean canChange) {
        this.canChange = canChange;
    }

    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public String getProtocolNr() {
        return protocolNr;
    }

    public void setProtocolNr(String protocolNr) {
        this.protocolNr = protocolNr;
    }

    public String getProtocolType() {
        return protocolType;
    }

    public void setProtocolType(String protocolType) {
        this.protocolType = protocolType;
    }

    public LocalDate getProtocolDate() {
        return protocolDate;
    }

    public void setProtocolDate(LocalDate protocolDate) {
        this.protocolDate = protocolDate;
    }

    public String getStatus() {
        return status;
    }

    public void setStatus(String status) {
        this.status = status;
    }

    public AutocompleteResult getSubject() {
        return subject;
    }

    public void setSubject(AutocompleteResult subject) {
        this.subject = subject;
    }

    public AutocompleteResult getModule() {
        return module;
    }

    public void setModule(AutocompleteResult module) {
        this.module = module;
    }

    public List<String> getTeachers() {
        return teachers;
    }

    public void setTeachers(List<String> teachers) {
        this.teachers = teachers;
    }

    public LocalDate getInserted() {
        return inserted;
    }

    public void setInserted(LocalDate inserted) {
        this.inserted = inserted;
    }

    public LocalDate getConfirmDate() {
        return confirmDate;
    }

    public void setConfirmDate(LocalDate confirmDate) {
        this.confirmDate = confirmDate;
    }

    public String getConfirmer() {
        return confirmer;
    }

    public void setConfirmer(String confirmer) {
        this.confirmer = confirmer;
    }

    public List<String> getStudentGroups() {
        return studentGroups;
    }

    public void setStudentGroups(List<String> studentGroups) {
        this.studentGroups = studentGroups;
    }
}
