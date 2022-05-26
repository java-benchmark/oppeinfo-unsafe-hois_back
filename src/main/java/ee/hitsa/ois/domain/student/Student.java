package ee.hitsa.ois.domain.student;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;

import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.OneToMany;
import javax.persistence.OneToOne;

import ee.hitsa.ois.domain.Classifier;
import ee.hitsa.ois.domain.Dormitory;
import ee.hitsa.ois.domain.FinalThesis;
import ee.hitsa.ois.domain.Person;
import ee.hitsa.ois.domain.PracticeJournal;
import ee.hitsa.ois.domain.directive.DirectiveStudent;
import ee.hitsa.ois.domain.school.School;
import ee.hitsa.ois.domain.timetable.JournalStudent;

@Entity
public class Student extends StudentBase {

    @ManyToOne(optional = false, fetch = FetchType.LAZY)
    @JoinColumn(nullable = false, updatable = false)
    private Person person;
    @ManyToOne(optional = false, fetch = FetchType.LAZY)
    private School school;
    private String previousSchoolName;
    private LocalDate previousSchoolEndDate;
    @Column(nullable = false)
    private Boolean isContractAgreed;
    private LocalDateTime contractAgreed;
    private String contractText;
    @ManyToOne(optional = false, fetch = FetchType.LAZY)
    private Classifier type;
    @OneToOne(cascade = { CascadeType.PERSIST, CascadeType.REMOVE }, fetch = FetchType.LAZY)
    private StudentHistory studentHistory;
    @OneToMany(mappedBy = "student")
    private List<StudentRepresentative> representatives;
    @OneToMany(mappedBy = "student")
    private List<JournalStudent> journalStudents;
    @OneToMany(mappedBy = "student")
    private List<StudentOccupationCertificate> occupationCertificates;
    @OneToMany(mappedBy = "student")
    private List<FinalThesis> finalThesis;
    @OneToMany(mappedBy = "student")
    private List<PracticeJournal> practiceJournals;
    @OneToMany(mappedBy = "student", cascade = CascadeType.ALL, orphanRemoval = true)
    private List<Dormitory> boardingSchools;
    @OneToMany(mappedBy = "student", cascade = CascadeType.ALL, orphanRemoval = true)
    private List<StudentSpecialNeed> specialNeeds;
    @OneToMany(mappedBy = "student", cascade = CascadeType.ALL, orphanRemoval = true)
    private List<DirectiveStudent> directiveStudents;
    @OneToMany(mappedBy = "student", cascade = CascadeType.ALL, orphanRemoval = true)
    private List<StudentSupportService> supportServices;
    @OneToMany(mappedBy = "student", cascade = CascadeType.ALL, orphanRemoval = true)
    private List<StudentLanguages> studentLanguages;
    private String addInfo;
    private String jobOccupation;
    private String job;
    private String otherContact;
    private Boolean isAcadStudyAllowed;
    private String representativeOtherContact;

    public Person getPerson() {
        return person;
    }

    public void setPerson(Person person) {
        this.person = person;
    }

    public School getSchool() {
        return school;
    }

    public void setSchool(School school) {
        this.school = school;
    }

    public String getPreviousSchoolName() {
        return previousSchoolName;
    }

    public void setPreviousSchoolName(String previousSchoolName) {
        this.previousSchoolName = previousSchoolName;
    }

    public LocalDate getPreviousSchoolEndDate() {
        return previousSchoolEndDate;
    }

    public void setPreviousSchoolEndDate(LocalDate previousSchoolEndDate) {
        this.previousSchoolEndDate = previousSchoolEndDate;
    }

    public Boolean getIsContractAgreed() {
        return isContractAgreed;
    }

    public void setIsContractAgreed(Boolean isContractAgreed) {
        this.isContractAgreed = isContractAgreed;
    }

    public LocalDateTime getContractAgreed() {
        return contractAgreed;
    }

    public void setContractAgreed(LocalDateTime contractAgreed) {
        this.contractAgreed = contractAgreed;
    }

    public String getContractText() {
        return contractText;
    }

    public void setContractText(String contractText) {
        this.contractText = contractText;
    }

    public StudentHistory getStudentHistory() {
        return studentHistory;
    }

    public void setStudentHistory(StudentHistory studentHistory) {
        this.studentHistory = studentHistory;
    }

    public List<StudentRepresentative> getRepresentatives() {
        return representatives;
    }

    public void setRepresentatives(List<StudentRepresentative> representatives) {
        this.representatives = representatives;
    }

    public List<JournalStudent> getJournalStudents() {
        return journalStudents;
    }

    public void setJournalStudents(List<JournalStudent> journalStudents) {
        this.journalStudents = journalStudents;
    }

    public List<StudentOccupationCertificate> getOccupationCertificates() {
        return occupationCertificates;
    }

    public void setOccupationCertificates(List<StudentOccupationCertificate> occupationCertificates) {
        this.occupationCertificates = occupationCertificates;
    }

    public List<FinalThesis> getFinalThesis() {
        return finalThesis != null ? finalThesis : new ArrayList<>();
    }

    public void setFinalThesis(List<FinalThesis> finalThesis) {
        this.finalThesis = finalThesis;
    }

    public List<PracticeJournal> getPracticeJournals() {
        return practiceJournals;
    }

    public void setPracticeJournals(List<PracticeJournal> practiceJournals) {
        this.practiceJournals = practiceJournals;
    }

    public List<Dormitory> getBoardingSchools() {
        return boardingSchools;
    }

    public void setBoardingSchools(List<Dormitory> boardingSchools) {
        this.boardingSchools = boardingSchools;
    }

    public List<StudentSpecialNeed> getSpecialNeeds() {
        return specialNeeds != null ? specialNeeds : (specialNeeds = new ArrayList<>());
    }

    public void setSpecialNeeds(List<StudentSpecialNeed> specialNeeds) {
        getSpecialNeeds().clear();
        getSpecialNeeds().addAll(specialNeeds);
    }

    public Classifier getType() {
        return type;
    }

    public void setType(Classifier type) {
        this.type = type;
    }

    public List<DirectiveStudent> getDirectiveStudents() {
        return directiveStudents;
    }

    public void setDirectiveStudents(List<DirectiveStudent> directiveStudents) {
        this.directiveStudents = directiveStudents;
    }

    public List<StudentSupportService> getSupportServices() {
        return supportServices != null ? supportServices : (supportServices = new ArrayList<>());
    }

    public void setSupportServices(List<StudentSupportService> supportServices) {
        getSupportServices().clear();
        getSupportServices().addAll(supportServices);
    }

    public String getAddInfo() {
        return addInfo;
    }

    public void setAddInfo(String addInfo) {
        this.addInfo = addInfo;
    }

    public String getJobOccupation() {
        return jobOccupation;
    }

    public void setJobOccupation(String jobOccupation) {
        this.jobOccupation = jobOccupation;
    }

    public String getJob() {
        return job;
    }

    public void setJob(String job) {
        this.job = job;
    }

    public String getOtherContact() {
        return otherContact;
    }

    public void setOtherContact(String otherContact) {
        this.otherContact = otherContact;
    }

    public Boolean getIsAcadStudyAllowed() {
        return isAcadStudyAllowed;
    }

    public void setIsAcadStudyAllowed(Boolean isAcadStudyAllowed) {
        this.isAcadStudyAllowed = isAcadStudyAllowed;
    }

    public String getRepresentativeOtherContact() {
        return representativeOtherContact;
    }

    public void setRepresentativeOtherContact(String representativeOtherContact) {
        this.representativeOtherContact = representativeOtherContact;
    }

    public List<StudentLanguages> getStudentLanguages() {
        return studentLanguages;
    }

    public void setStudentLanguages(List<StudentLanguages> studentLanguages) {
        this.studentLanguages = studentLanguages;
    }
    
}
