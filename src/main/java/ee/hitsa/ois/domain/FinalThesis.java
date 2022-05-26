package ee.hitsa.ois.domain;

import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import javax.persistence.CascadeType;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.OneToMany;

import ee.hitsa.ois.domain.curriculum.CurriculumGrade;
import ee.hitsa.ois.domain.student.Student;

@Entity
public class FinalThesis extends BaseEntityWithId {
    
    @ManyToOne(optional = false, fetch = FetchType.LAZY)
    @JoinColumn(nullable = false)
    private Student student;
    
    private String themeEt;
    private String themeEn;
    private Boolean hasDraft;
    private String draft;
    
    @ManyToOne(optional = false, fetch = FetchType.LAZY)
    private Classifier status;
    
    private String confirmedBy;
    private LocalDateTime confirmed;
    private String addInfo;
    
    @ManyToOne(fetch = FetchType.LAZY)
    private Classifier language;
    @ManyToOne(fetch = FetchType.LAZY)
    private CurriculumGrade curriculumGrade;
    
    @OneToMany(mappedBy="finalThesis", cascade = CascadeType.ALL, orphanRemoval = true)
    private List<FinalThesisSupervisor> supervisors = new ArrayList<>();
    @OneToMany(mappedBy="finalThesis", cascade = CascadeType.ALL, orphanRemoval = true)
    private Set<FinalThesisCercs> cercses;

    public Student getStudent() {
        return student;
    }

    public void setStudent(Student student) {
        this.student = student;
    }

    public String getThemeEt() {
        return themeEt;
    }

    public void setThemeEt(String themeEt) {
        this.themeEt = themeEt;
    }

    public String getThemeEn() {
        return themeEn;
    }

    public void setThemeEn(String themeEn) {
        this.themeEn = themeEn;
    }

    public Boolean getHasDraft() {
        return hasDraft;
    }

    public void setHasDraft(Boolean hasDraft) {
        this.hasDraft = hasDraft;
    }

    public String getDraft() {
        return draft;
    }

    public void setDraft(String draft) {
        this.draft = draft;
    }

    public Classifier getStatus() {
        return status;
    }

    public void setStatus(Classifier status) {
        this.status = status;
    }

    public String getConfirmedBy() {
        return confirmedBy;
    }

    public void setConfirmedBy(String confirmedBy) {
        this.confirmedBy = confirmedBy;
    }

    public LocalDateTime getConfirmed() {
        return confirmed;
    }

    public void setConfirmed(LocalDateTime confirmed) {
        this.confirmed = confirmed;
    }

    public String getAddInfo() {
        return addInfo;
    }

    public void setAddInfo(String addInfo) {
        this.addInfo = addInfo;
    }

    public List<FinalThesisSupervisor> getSupervisors() {
        return supervisors;
    }

    public void setSupervisors(List<FinalThesisSupervisor> supervisors) {
        this.supervisors = supervisors;
    }

    public Classifier getLanguage() {
        return language;
    }

    public void setLanguage(Classifier language) {
        this.language = language;
    }

    public CurriculumGrade getCurriculumGrade() {
        return curriculumGrade;
    }

    public void setCurriculumGrade(CurriculumGrade curriculumGrade) {
        this.curriculumGrade = curriculumGrade;
    }

    public Set<FinalThesisCercs> getCercses() {
        return cercses != null ? cercses : (cercses = new HashSet<>());
    }

    public void setCercses(Set<FinalThesisCercs> cercses) {
        getCercses().clear();
        getCercses().addAll(cercses);
    }
    
}
