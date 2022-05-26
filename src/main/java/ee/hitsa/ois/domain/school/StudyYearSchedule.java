package ee.hitsa.ois.domain.school;

import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;

import ee.hitsa.ois.domain.BaseEntityWithId;
import ee.hitsa.ois.domain.StudyPeriod;
import ee.hitsa.ois.domain.student.StudentGroup;

@Entity
public class StudyYearSchedule extends BaseEntityWithId {

    @ManyToOne(optional = false, fetch = FetchType.LAZY)
    @JoinColumn(name = "school_id", updatable = false)
    private School school;
    @ManyToOne(optional = false, fetch = FetchType.LAZY)
    @JoinColumn(name = "student_group_id", updatable = false)
    private StudentGroup studentGroup;
    @ManyToOne(optional = false, fetch = FetchType.LAZY)
    @JoinColumn(name = "study_year_schedule_legend_id", updatable = true)
    private StudyYearScheduleLegend studyYearScheduleLegend;
    @ManyToOne(optional = false, fetch = FetchType.LAZY)
    @JoinColumn(name = "study_period_id", updatable = false)
    private StudyPeriod studyPeriod;
    private Short weekNr;
    private String addInfo;

    public School getSchool() {
        return school;
    }

    public void setSchool(School school) {
        this.school = school;
    }

    public StudentGroup getStudentGroup() {
        return studentGroup;
    }

    public void setStudentGroup(StudentGroup studentGroup) {
        this.studentGroup = studentGroup;
    }

    public StudyYearScheduleLegend getStudyYearScheduleLegend() {
        return studyYearScheduleLegend;
    }

    public void setStudyYearScheduleLegend(StudyYearScheduleLegend studyYearScheduleLegend) {
        this.studyYearScheduleLegend = studyYearScheduleLegend;
    }

    public StudyPeriod getStudyPeriod() {
        return studyPeriod;
    }

    public void setStudyPeriod(StudyPeriod studyPeriod) {
        this.studyPeriod = studyPeriod;
    }

    public Short getWeekNr() {
        return weekNr;
    }

    public void setWeekNr(Short weekNr) {
        this.weekNr = weekNr;
    }

    public String getAddInfo() {
        return addInfo;
    }

    public void setAddInfo(String addInfo) {
        this.addInfo = addInfo;
    }

}
