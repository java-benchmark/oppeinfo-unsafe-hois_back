package ee.hitsa.ois.domain;

import java.time.LocalDate;

import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.validation.constraints.NotNull;

@Entity
public class PracticeJournalEntry extends BaseEntityWithId {

    @ManyToOne(optional = false, fetch = FetchType.LAZY)
    @JoinColumn(nullable = false, updatable = false, insertable = false)
    private PracticeJournal practiceJournal;

    private String description;

    @NotNull
    private LocalDate practiceDate;
    /**
     * @since 23.01.2020
     * Saves a value as minutes, not as hours.
     */
    private Double hours;
    private String supervisorComment;
    private String teacherComment;

    public PracticeJournal getPracticeJournal() {
        return practiceJournal;
    }

    public void setPracticeJournal(PracticeJournal practiceJournal) {
        this.practiceJournal = practiceJournal;
    }

    public String getDescription() {
        return description;
    }

    public void setDescription(String description) {
        this.description = description;
    }

    public LocalDate getPracticeDate() {
        return practiceDate;
    }

    public void setPracticeDate(LocalDate practiceDate) {
        this.practiceDate = practiceDate;
    }

    public Double getHours() {
        return hours;
    }

    public void setHours(Double hours) {
        this.hours = hours;
    }

    public String getSupervisorComment() {
        return supervisorComment;
    }

    public void setSupervisorComment(String supervisorComment) {
        this.supervisorComment = supervisorComment;
    }

    public String getTeacherComment() {
        return teacherComment;
    }

    public void setTeacherComment(String teacherComment) {
        this.teacherComment = teacherComment;
    }

}
