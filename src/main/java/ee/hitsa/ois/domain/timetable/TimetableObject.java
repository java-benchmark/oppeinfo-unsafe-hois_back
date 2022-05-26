package ee.hitsa.ois.domain.timetable;

import java.util.ArrayList;
import java.util.List;

import javax.persistence.CascadeType;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.OneToMany;

import ee.hitsa.ois.domain.BaseEntityWithId;
import ee.hitsa.ois.domain.subject.studyperiod.SubjectStudyPeriod;

@Entity
public class TimetableObject extends BaseEntityWithId {

    @ManyToOne(optional = false, fetch = FetchType.LAZY)
    @JoinColumn(nullable = false, updatable = false)
    private Timetable timetable;
    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(updatable = false)
    private Journal journal;
    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(updatable = false)
    private SubjectStudyPeriod subjectStudyPeriod;
    @OneToMany(cascade = CascadeType.ALL, orphanRemoval = true)
    @JoinColumn(name = "timetable_object_id", nullable = true, updatable = true)
    private List<TimetableEvent> timetableEvents = new ArrayList<>();
    @OneToMany(cascade = CascadeType.ALL, orphanRemoval = true)
    @JoinColumn(name = "timetable_object_id", nullable = false, updatable = false, insertable = false)
    private List<TimetableObjectStudentGroup> timetableObjectStudentGroups = new ArrayList<>();

    public Timetable getTimetable() {
        return timetable;
    }

    public void setTimetable(Timetable timetable) {
        this.timetable = timetable;
    }

    public Journal getJournal() {
        return journal;
    }

    public void setJournal(Journal journal) {
        this.journal = journal;
    }

    public SubjectStudyPeriod getSubjectStudyPeriod() {
        return subjectStudyPeriod;
    }

    public void setSubjectStudyPeriod(SubjectStudyPeriod subjectStudyPeriod) {
        this.subjectStudyPeriod = subjectStudyPeriod;
    }

    public List<TimetableEvent> getTimetableEvents() {
        return timetableEvents;
    }

    public void setTimetableEvents(List<TimetableEvent> timetableEvents) {
        this.timetableEvents = timetableEvents;
    }

    public List<TimetableObjectStudentGroup> getTimetableObjectStudentGroups() {
        return timetableObjectStudentGroups;
    }

    public void setTimetableObjectStudentGroups(List<TimetableObjectStudentGroup> timetableObjectStudentGroups) {
        this.timetableObjectStudentGroups = timetableObjectStudentGroups;
    }
    
    

}
