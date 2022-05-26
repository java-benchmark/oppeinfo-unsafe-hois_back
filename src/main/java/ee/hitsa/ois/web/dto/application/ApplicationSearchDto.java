package ee.hitsa.ois.web.dto.application;

import java.time.LocalDateTime;

import ee.hitsa.ois.domain.application.Application;
import ee.hitsa.ois.util.EntityUtil;
import ee.hitsa.ois.web.dto.AutocompleteResult;

public class ApplicationSearchDto {

    private Long id;
    private AutocompleteResult student;
    private String studentGroup;
    private String status;
    private String rejectReason;
    private String type;
    private LocalDateTime inserted;
    private LocalDateTime submitted;
    private Boolean canViewStudent;
    private Boolean canEditStudent;
    private Boolean isConnectedByCommittee;

    public static ApplicationSearchDto of(Application application) {
        ApplicationSearchDto dto = EntityUtil.bindToDto(application, new ApplicationSearchDto());
        return dto;
    }

    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public AutocompleteResult getStudent() {
        return student;
    }

    public void setStudent(AutocompleteResult student) {
        this.student = student;
    }

    public String getStatus() {
        return status;
    }

    public void setStatus(String status) {
        this.status = status;
    }

    public String getRejectReason() {
        return rejectReason;
    }

    public void setRejectReason(String rejectReason) {
        this.rejectReason = rejectReason;
    }

    public String getType() {
        return type;
    }

    public void setType(String type) {
        this.type = type;
    }

    public LocalDateTime getInserted() {
        return inserted;
    }

    public void setInserted(LocalDateTime inserted) {
        this.inserted = inserted;
    }

    public LocalDateTime getSubmitted() {
        return submitted;
    }

    public void setSubmitted(LocalDateTime submitted) {
        this.submitted = submitted;
    }

    public Boolean getCanViewStudent() {
        return canViewStudent;
    }

    public void setCanViewStudent(Boolean canViewStudent) {
        this.canViewStudent = canViewStudent;
    }

    public Boolean getCanEditStudent() {
        return canEditStudent;
    }

    public void setCanEditStudent(Boolean canEditStudent) {
        this.canEditStudent = canEditStudent;
    }

    public String getStudentGroup() {
        return studentGroup;
    }

    public void setStudentGroup(String studentGroup) {
        this.studentGroup = studentGroup;
    }

    public Boolean getIsConnectedByCommittee() {
        return isConnectedByCommittee;
    }

    public void setIsConnectedByCommittee(Boolean isConnectedByCommittee) {
        this.isConnectedByCommittee = isConnectedByCommittee;
    }

}
