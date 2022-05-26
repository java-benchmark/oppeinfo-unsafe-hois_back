package ee.hitsa.ois.report.studentgroupteacher;

import java.time.LocalDate;

import ee.hitsa.ois.web.dto.AutocompleteResult;

public class ResultReport {

    private Long studentId;
    private String firstname;
    private String lastname;
    private String fullname;
    private AutocompleteResult journal;
    private String entryType;
    private String grade;
    private LocalDate gradeInserted;
    private String gradeInsertedBy;
    private Boolean isPracticeJournal;

    public Long getStudentId() {
        return studentId;
    }

    public void setStudentId(Long studentId) {
        this.studentId = studentId;
    }

    public String getFirstname() {
        return firstname;
    }

    public void setFirstname(String firstname) {
        this.firstname = firstname;
    }

    public String getLastname() {
        return lastname;
    }

    public void setLastname(String lastname) {
        this.lastname = lastname;
    }

    public AutocompleteResult getJournal() {
        return journal;
    }

    public void setJournal(AutocompleteResult journal) {
        this.journal = journal;
    }

    public String getEntryType() {
        return entryType;
    }

    public void setEntryType(String entryType) {
        this.entryType = entryType;
    }

    public String getGrade() {
        return grade;
    }

    public void setGrade(String grade) {
        this.grade = grade;
    }

    public LocalDate getGradeInserted() {
        return gradeInserted;
    }

    public void setGradeInserted(LocalDate gradeInserted) {
        this.gradeInserted = gradeInserted;
    }

    public String getGradeInsertedBy() {
        return gradeInsertedBy;
    }

    public void setGradeInsertedBy(String gradeInsertedBy) {
        this.gradeInsertedBy = gradeInsertedBy;
    }

    public Boolean getIsPracticeJournal() {
        return isPracticeJournal;
    }

    public void setIsPracticeJournal(Boolean isPracticeJournal) {
        this.isPracticeJournal = isPracticeJournal;
    }

    public String getFullname() {
        return fullname;
    }

    public void setFullname(String fullname) {
        this.fullname = fullname;
    }

}
