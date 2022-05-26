package ee.hitsa.ois.web.commandobject.student;

import java.util.List;

import javax.validation.constraints.Size;

public class StudentSearchCommand {

    @Size(max = 255)
    private String name;
    private String idcode;
    private List<Long> curriculum;
    private List<Long> curriculumVersion;
    @Size(max = 100)
    private String studentGroup;
    private List<Long> studentGroupId;
    private List<Long> journalId;
    private List<Long> subjectId;
    private List<String> studyForm;
    private List<String> status;
    private Boolean higher;
    private Boolean showMyStudentGroups;
    private Long studentType;

    public List<Long> getStudentGroupId() {
        return studentGroupId;
    }

    public void setStudentGroupId(List<Long> studentGroupId) {
        this.studentGroupId = studentGroupId;
    }

    public List<Long> getCurriculum() {
        return curriculum;
    }

    public void setCurriculum(List<Long> curriculum) {
        this.curriculum = curriculum;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public String getIdcode() {
        return idcode;
    }

    public void setIdcode(String idcode) {
        this.idcode = idcode;
    }

    public List<Long> getCurriculumVersion() {
        return curriculumVersion;
    }

    public void setCurriculumVersion(List<Long> curriculumVersion) {
        this.curriculumVersion = curriculumVersion;
    }

    public String getStudentGroup() {
        return studentGroup;
    }

    public void setStudentGroup(String studentGroup) {
        this.studentGroup = studentGroup;
    }

    public List<String> getStudyForm() {
        return studyForm;
    }

    public void setStudyForm(List<String> studyForm) {
        this.studyForm = studyForm;
    }

    public List<String> getStatus() {
        return status;
    }

    public void setStatus(List<String> status) {
        this.status = status;
    }

    public List<Long> getJournalId() {
        return journalId;
    }

    public void setJournalId(List<Long> journalId) {
        this.journalId = journalId;
    }

    public List<Long> getSubjectId() {
        return subjectId;
    }

    public void setSubjectId(List<Long> subjectId) {
        this.subjectId = subjectId;
    }

    public Boolean getHigher() {
        return higher;
    }

    public void setHigher(Boolean higher) {
        this.higher = higher;
    }

    public Boolean getShowMyStudentGroups() {
        return showMyStudentGroups;
    }

    public void setShowMyStudentGroups(Boolean showMyStudentGroups) {
        this.showMyStudentGroups = showMyStudentGroups;
    }

    public Long getStudentType() {
        return studentType;
    }

    public void setStudentType(Long studentType) {
        this.studentType = studentType;
    }

}
