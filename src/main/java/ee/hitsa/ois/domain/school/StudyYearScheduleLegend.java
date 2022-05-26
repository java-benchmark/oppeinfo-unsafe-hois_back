package ee.hitsa.ois.domain.school;

import java.util.ArrayList;
import java.util.List;

import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.OneToMany;

import ee.hitsa.ois.domain.BaseEntityWithId;

@Entity
public class StudyYearScheduleLegend extends BaseEntityWithId {

    private String code;
    private String nameEt;
    private String nameEn;
    private String color;
    @ManyToOne(optional = false, fetch = FetchType.LAZY)
    @JoinColumn(nullable = false, updatable = false)
    private School school;
    @OneToMany(orphanRemoval = true, mappedBy = "studyYearScheduleLegend")
    private List<StudyYearSchedule> studyYearSchedules = new ArrayList<>();

    public String getCode() {
        return code;
    }

    public void setCode(String code) {
        this.code = code;
    }

    public String getNameEt() {
        return nameEt;
    }

    public void setNameEt(String nameEt) {
        this.nameEt = nameEt;
    }

    public String getNameEn() {
        return nameEn;
    }

    public void setNameEn(String nameEn) {
        this.nameEn = nameEn;
    }

    public String getColor() {
        return color;
    }

    public void setColor(String color) {
        this.color = color;
    }

    public School getSchool() {
        return school;
    }

    public void setSchool(School school) {
        this.school = school;
    }

    public List<StudyYearSchedule> getStudyYearSchedules() {
        return studyYearSchedules;
    }

    public void setStudyYearSchedules(List<StudyYearSchedule> studyYearSchedules) {
        this.studyYearSchedules = studyYearSchedules;
    }

}
