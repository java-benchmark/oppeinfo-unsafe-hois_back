package ee.hitsa.ois.web.commandobject.curriculum;

import ee.hitsa.ois.web.commandobject.SearchCommand;

public class CurriculumVersionAutocompleteCommand extends SearchCommand {

    private Boolean higher;
    private Boolean vocational;
    private Boolean sais;
    private Boolean valid;
    private Boolean closed;
    private Boolean languages;
    private Long curriculumId;
    private Boolean hasGroup;
    private String studyForm;
    private Boolean checkStudentGroupStudyForm;
    private Long userId;

    public Boolean getHigher() {
        return higher;
    }

    public void setHigher(Boolean higher) {
        this.higher = higher;
    }

    public Boolean getSais() {
        return sais;
    }

    public void setSais(Boolean sais) {
        this.sais = sais;
    }

    public Boolean getValid() {
        return valid;
    }

    public void setValid(Boolean valid) {
        this.valid = valid;
    }
    
    public Boolean getClosed() {
        return closed;
    }

    public void setClosed(Boolean closed) {
        this.closed = closed;
    }

    public Boolean getLanguages() {
        return languages;
    }

    public void setLanguages(Boolean languages) {
        this.languages = languages;
    }

    public Long getCurriculumId() {
        return curriculumId;
    }

    public void setCurriculumId(Long curriculumId) {
        this.curriculumId = curriculumId;
    }

    public Boolean getHasGroup() {
        return hasGroup;
    }

    public void setHasGroup(Boolean hasGroup) {
        this.hasGroup = hasGroup;
    }

    public String getStudyForm() {
        return studyForm;
    }

    public void setStudyForm(String studyForm) {
        this.studyForm = studyForm;
    }

    public Boolean getCheckStudentGroupStudyForm() {
        return checkStudentGroupStudyForm;
    }

    public void setCheckStudentGroupStudyForm(Boolean checkStudentGroupStudyForm) {
        this.checkStudentGroupStudyForm = checkStudentGroupStudyForm;
    }

    public Long getUserId() {
        return userId;
    }

    public void setUserId(Long userId) {
        this.userId = userId;
    }

    public Boolean getVocational() {
        return vocational;
    }

    public void setVocational(Boolean vocational) {
        this.vocational = vocational;
    }

}
