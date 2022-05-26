package ee.hitsa.ois.web.commandobject;

import java.time.LocalDate;
import java.util.List;

import javax.validation.Valid;
import javax.validation.constraints.NotNull;
import javax.validation.constraints.Size;

import org.hibernate.validator.constraints.NotEmpty;

import ee.hitsa.ois.enums.MainClassCode;
import ee.hitsa.ois.validation.ClassifierRestriction;
import ee.hitsa.ois.validation.DateRange;
import ee.hitsa.ois.web.dto.AutocompleteResult;

@DateRange(from = "startDate", thru = "endDate")
public class PracticeJournalForm extends VersionedCommand {

    @NotNull
    private AutocompleteResult student;
    @NotNull
    private LocalDate startDate;
    @NotNull
    private LocalDate endDate;
    @NotNull
    @Size(max = 255)
    private String practicePlace;
    @NotNull
    private AutocompleteResult teacher;
    
    private Long practiceEvaluation;
    @NotNull
    @Size(max = 20000)
    private String practicePlan;
    private Boolean isHigher;
    @NotEmpty
    @Valid
    private List<PracticeJournalModuleSubjectForm> moduleSubjects;
    @ClassifierRestriction({MainClassCode.KUTSEHINDAMINE, MainClassCode.KORGHINDAMINE})
    private String grade;

    public AutocompleteResult getStudent() {
        return student;
    }

    public void setStudent(AutocompleteResult student) {
        this.student = student;
    }

    public LocalDate getStartDate() {
        return startDate;
    }

    public void setStartDate(LocalDate startDate) {
        this.startDate = startDate;
    }

    public LocalDate getEndDate() {
        return endDate;
    }

    public void setEndDate(LocalDate endDate) {
        this.endDate = endDate;
    }

    public String getPracticePlace() {
        return practicePlace;
    }

    public void setPracticePlace(String practicePlace) {
        this.practicePlace = practicePlace;
    }

    public AutocompleteResult getTeacher() {
        return teacher;
    }

    public void setTeacher(AutocompleteResult teacher) {
        this.teacher = teacher;
    }

    public String getPracticePlan() {
        return practicePlan;
    }

    public void setPracticePlan(String practicePlan) {
        this.practicePlan = practicePlan;
    }

    public Boolean getIsHigher() {
        return isHigher;
    }

    public void setIsHigher(Boolean isHigher) {
        this.isHigher = isHigher;
    }

    public List<PracticeJournalModuleSubjectForm> getModuleSubjects() {
        return moduleSubjects;
    }

    public void setModuleSubjects(List<PracticeJournalModuleSubjectForm> moduleSubjects) {
        this.moduleSubjects = moduleSubjects;
    }

    public Long getPracticeEvaluation() {
        return practiceEvaluation;
    }

    public void setPracticeEvaluation(Long practiceEvaluation) {
        this.practiceEvaluation = practiceEvaluation;
    }

    public String getGrade() {
        return grade;
    }

    public void setGrade(String grade) {
        this.grade = grade;
    }

}
