package ee.hitsa.ois.web.dto.student;

import java.time.LocalDate;
import java.util.List;

import ee.hitsa.ois.web.dto.AutocompleteResult;

public class StudentGroupSearchDto {

    private Long id;
    private String code;
    private AutocompleteResult curriculum;
    private Short curriculumVersionAdmissinYear;
    private String studyForm;
    private String teacher;
    private Integer course;
    private Boolean higher;
    private Long studentCount;
    private List<Long> schoolDepartments;
    // Planned number of hours in studyPeriod
    private Long hours;
    private LocalDate validFrom;
    private LocalDate validThru;
    private Boolean canEdit;

    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public String getCode() {
        return code;
    }

    public void setCode(String code) {
        this.code = code;
    }

    public AutocompleteResult getCurriculum() {
        return curriculum;
    }

    public void setCurriculum(AutocompleteResult curriculum) {
        this.curriculum = curriculum;
    }

    public Short getCurriculumVersionAdmissinYear() {
        return curriculumVersionAdmissinYear;
    }

    public void setCurriculumVersionAdmissinYear(Short curriculumVersionAdmissinYear) {
        this.curriculumVersionAdmissinYear = curriculumVersionAdmissinYear;
    }

    public Boolean isHigher() {
        return higher;
    }
    
    public void setHigher(Boolean higher) {
        this.higher = higher;
    }

    public String getStudyForm() {
        return studyForm;
    }

    public void setStudyForm(String studyForm) {
        this.studyForm = studyForm;
    }

    public String getTeacher() {
        return teacher;
    }

    public void setTeacher(String teacher) {
        this.teacher = teacher;
    }

    public Integer getCourse() {
        return course;
    }

    public void setCourse(Integer course) {
        this.course = course;
    }

    public Long getStudentCount() {
        return studentCount;
    }

    public void setStudentCount(Long studentCount) {
        this.studentCount = studentCount;
    }

    public List<Long> getSchoolDepartments() {
        return schoolDepartments;
    }

    public void setSchoolDepartments(List<Long> schoolDepartments) {
        this.schoolDepartments = schoolDepartments;
    }

    public Long getHours() {
        return hours;
    }

    public void setHours(Long hours) {
        this.hours = hours;
    }

    public LocalDate getValidFrom() {
        return validFrom;
    }

    public void setValidFrom(LocalDate validFrom) {
        this.validFrom = validFrom;
    }

    public LocalDate getValidThru() {
        return validThru;
    }

    public void setValidThru(LocalDate validThru) {
        this.validThru = validThru;
    }

    public Boolean getCanEdit() {
        return canEdit;
    }

    public void setCanEdit(Boolean canEdit) {
        this.canEdit = canEdit;
    }

}
