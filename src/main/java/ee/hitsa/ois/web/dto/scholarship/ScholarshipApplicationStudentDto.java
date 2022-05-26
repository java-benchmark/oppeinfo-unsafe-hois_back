package ee.hitsa.ois.web.dto.scholarship;

import ee.hitsa.ois.web.dto.AutocompleteResult;

import java.time.LocalDate;

public class ScholarshipApplicationStudentDto extends ScholarshipApplicationBaseDto {
    private Long termId;
    private String type;
    private Boolean isTeacherConfirm;
    private LocalDate decisionDate;
    private String rejectComment;
    private Boolean needsConfirm;
    private String termName;
    private AutocompleteResult termStudyPeriod;
    private Boolean canApply;

    public Long getTermId() {
        return termId;
    }

    public void setTermId(Long termId) {
        this.termId = termId;
    }

    public String getType() {
        return type;
    }

    public void setType(String type) {
        this.type = type;
    }

    public Boolean getIsTeacherConfirm() {
        return isTeacherConfirm;
    }

    public void setIsTeacherConfirm(Boolean isTeacherConfirm) {
        this.isTeacherConfirm = isTeacherConfirm;
    }

    public LocalDate getDecisionDate() {
        return decisionDate;
    }

    public void setDecisionDate(LocalDate decisionDate) {
        this.decisionDate = decisionDate;
    }

    public String getRejectComment() {
        return rejectComment;
    }

    public void setRejectComment(String rejectComment) {
        this.rejectComment = rejectComment;
    }

    public void setNeedsConfirm(Boolean needsConfirm) {
        this.needsConfirm = needsConfirm;
    }

    public Boolean getNeedsConfirm() {
        return this.needsConfirm;
    }

    public String getTermName() {
        return termName;
    }

    public void setTermName(String termName) {
        this.termName = termName;
    }

    public AutocompleteResult getTermStudyPeriod() {
        return termStudyPeriod;
    }

    public void setTermStudyPeriod(AutocompleteResult termStudyPeriod) {
        this.termStudyPeriod = termStudyPeriod;
    }

    public Boolean getCanApply() {
        return canApply;
    }

    public void setCanApply(Boolean canApply) {
        this.canApply = canApply;
    }

}
