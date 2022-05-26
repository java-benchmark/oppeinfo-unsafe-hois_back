package ee.hitsa.ois.web.commandobject.directive;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.List;
import java.util.Map;

import javax.validation.Valid;
import javax.validation.constraints.Max;
import javax.validation.constraints.Min;
import javax.validation.constraints.Size;
import javax.validation.groups.Default;

import ee.hitsa.ois.domain.directive.DirectiveStudentModule;
import ee.hitsa.ois.enums.MainClassCode;
import ee.hitsa.ois.util.EntityUtil;
import ee.hitsa.ois.validation.ClassifierRestriction;
import ee.hitsa.ois.validation.DateRange;
import ee.hitsa.ois.validation.DirectiveValidation.Duplikaat;
import ee.hitsa.ois.validation.DirectiveValidation.Immat;
import ee.hitsa.ois.validation.DirectiveValidation.Kylalis;
import ee.hitsa.ois.validation.DirectiveValidation.Muu;
import ee.hitsa.ois.validation.EstonianIdCode;
import ee.hitsa.ois.validation.Required;
import ee.hitsa.ois.validation.StudyPeriodRange;
import ee.hitsa.ois.web.commandobject.VersionedCommand;
import ee.hitsa.ois.web.dto.AutocompleteResult;
import ee.hitsa.ois.web.dto.directive.ExistingDirectiveStudentDto;
import ee.hitsa.ois.web.dto.directive.ScholarshipApplicationSelectDto;

public class DirectiveForm extends VersionedCommand {

    @Required
    @ClassifierRestriction(MainClassCode.KASKKIRI)
    private String type;
    private Boolean isHigher;
    @Required
    @Size(max = 500)
    private String headline;
    @Size(max = 4000)
    private String addInfo;
    private Long directiveCoordinator;
    private Long canceledDirective;
    @ClassifierRestriction(MainClassCode.KASKKIRI_TYHISTAMISE_VIIS)
    private String cancelType;
    @ClassifierRestriction(MainClassCode.STIPTOETUS)
    private String scholarshipType;
    @ClassifierRestriction(MainClassCode.EHIS_STIPENDIUM)
    private String scholarshipEhis;
    @Valid
    private List<? extends DirectiveFormStudent> students;
    private List<Long> selectedStudents;

    public String getType() {
        return type;
    }

    public void setType(String type) {
        this.type = type;
    }

    public Boolean getIsHigher() {
        return isHigher;
    }

    public void setIsHigher(Boolean isHigher) {
        this.isHigher = isHigher;
    }

    public String getHeadline() {
        return headline;
    }

    public void setHeadline(String headline) {
        this.headline = headline;
    }

    public String getAddInfo() {
        return addInfo;
    }

    public void setAddInfo(String addInfo) {
        this.addInfo = addInfo;
    }

    public Long getDirectiveCoordinator() {
        return directiveCoordinator;
    }

    public void setDirectiveCoordinator(Long directiveCoordinator) {
        this.directiveCoordinator = directiveCoordinator;
    }

    public Long getCanceledDirective() {
        return canceledDirective;
    }

    public void setCanceledDirective(Long canceledDirective) {
        this.canceledDirective = canceledDirective;
    }

    public String getCancelType() {
        return cancelType;
    }

    public void setCancelType(String cancelType) {
        this.cancelType = cancelType;
    }

    public String getScholarshipType() {
        return scholarshipType;
    }

    public void setScholarshipType(String scholarshipType) {
        this.scholarshipType = scholarshipType;
    }

    public String getScholarshipEhis() {
        return scholarshipEhis;
    }

    public void setScholarshipEhis(String scholarshipEhis) {
        this.scholarshipEhis = scholarshipEhis;
    }

    public List<? extends DirectiveFormStudent> getStudents() {
        return students;
    }

    public void setStudents(List<? extends DirectiveFormStudent> students) {
        this.students = students;
    }

    public List<Long> getSelectedStudents() {
        return selectedStudents;
    }

    public void setSelectedStudents(List<Long> selectedStudents) {
        this.selectedStudents = selectedStudents;
    }

    @DateRange(from = "startDate", thru = "endDate")
    @StudyPeriodRange(from = "studyPeriodStart", thru = "studyPeriodEnd")
    public static class DirectiveFormStudent {
        private Long id;
        @EstonianIdCode(groups = {Immat.class, Kylalis.class})
        private String idcode;
        private String foreignIdcode;
        @Required(groups = {Immat.class, Kylalis.class})
        @Size(max = 100, groups = {Immat.class, Kylalis.class}, message = "maxlength")
        private String firstname;
        @Required(groups = {Immat.class, Kylalis.class})
        @Size(max = 100, groups = {Immat.class, Kylalis.class}, message = "maxlength")
        private String lastname;
        @Required(groups = {Immat.class, Kylalis.class})
        private LocalDate birthdate;
        @ClassifierRestriction(MainClassCode.SUGU)
        private String sex;
        @ClassifierRestriction(MainClassCode.RIIK)
        private String citizenship;
        private Long apelSchoolId;
        private LocalDate startDate;
        private LocalDate endDate;
        @ClassifierRestriction({MainClassCode.AKADPUHKUS_POHJUS, MainClassCode.EKSMAT_POHJUS, MainClassCode.KASKKIRI_STIPTOETL_POHJUS})
        private String reason;
        @ClassifierRestriction(MainClassCode.OPPEVORM)
        private String studyForm;
        private Long studentGroup;
        private Long curriculumVersion;
        @ClassifierRestriction(MainClassCode.OPPEKOORMUS)
        private String studyLoad;
        @ClassifierRestriction(MainClassCode.FINALLIKAS)
        private String fin;
        @ClassifierRestriction(MainClassCode.FINTAPSUSTUS)
        private String finSpecific;
        @ClassifierRestriction(MainClassCode.OPPEKEEL)
        private String language;
        @ClassifierRestriction({MainClassCode.OPPEASTE, MainClassCode.EHIS_KODU_OPPEASTE})
        private String previousStudyLevel;
        private String email;
        private Boolean isPeriod;
        private Long studyPeriodStart;
        private Long studyPeriodEnd;
        private LocalDate nominalStudyEnd;
        private Boolean isAbroad;
        @ClassifierRestriction(MainClassCode.EHIS_KOOL)
        private String ehisSchool;
        private String abroadSchool;
        @ClassifierRestriction(MainClassCode.RIIK)
        private String country;
        @ClassifierRestriction(MainClassCode.VALISOPE_EESMARK)
        private String abroadPurpose;
        @ClassifierRestriction(MainClassCode.VALISKOOL_PROGRAMM)
        private String abroadProgramme;
        @Min(0)
        @Max(9999) // EHIS does not allow more than 9999,99
        private BigDecimal amountPaid;
        private String bankAccount;
        private Long application;
        private Long student;
        private Long saisApplication;
        private Long scholarshipApplication;
        @Size.List({
                @Size(max = 255, groups = {Muu.class}, message = "maxlength"),
                @Size(max = 4000, groups = {Default.class}, message = "maxlength"),
        })
        private String addInfo;
        private Boolean isAbsence;
        private List<String> occupations;
        private Map<String, List<String>> specialities;
        private List<DirectiveFormStudentModule> modules;
        private List<ScholarshipApplicationSelectDto> scholarshipApplications;
        private List<ExistingDirectiveStudentDto> existingDirectiveStudents;
        private Long directiveStudent;
        @ClassifierRestriction(MainClassCode.YHISELAMU)
        private String dormitory;
        
        private Boolean diplomaChk;
        private Boolean diplomaSupplementChk;
        private Boolean diplomaSupplementEnChk;

        public Long getId() {
            return id;
        }

        public void setId(Long id) {
            this.id = id;
        }

        public String getIdcode() {
            return idcode;
        }

        public void setIdcode(String idcode) {
            this.idcode = idcode;
        }

        public String getForeignIdcode() {
            return foreignIdcode;
        }

        public void setForeignIdcode(String foreignIdcode) {
            this.foreignIdcode = foreignIdcode;
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

        public LocalDate getBirthdate() {
            return birthdate;
        }

        public void setBirthdate(LocalDate birthdate) {
            this.birthdate = birthdate;
        }

        public String getSex() {
            return sex;
        }

        public void setSex(String sex) {
            this.sex = sex;
        }

        public String getCitizenship() {
            return citizenship;
        }

        public void setCitizenship(String citizenship) {
            this.citizenship = citizenship;
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

        public String getReason() {
            return reason;
        }

        public void setReason(String reason) {
            this.reason = reason;
        }

        public String getStudyForm() {
            return studyForm;
        }

        public void setStudyForm(String studyForm) {
            this.studyForm = studyForm;
        }

        public Long getStudentGroup() {
            return studentGroup;
        }

        public void setStudentGroup(Long studentGroup) {
            this.studentGroup = studentGroup;
        }

        public Long getCurriculumVersion() {
            return curriculumVersion;
        }

        public void setCurriculumVersion(Long curriculumVersion) {
            this.curriculumVersion = curriculumVersion;
        }

        public String getStudyLoad() {
            return studyLoad;
        }

        public void setStudyLoad(String studyLoad) {
            this.studyLoad = studyLoad;
        }

        public String getFin() {
            return fin;
        }

        public void setFin(String fin) {
            this.fin = fin;
        }

        public String getFinSpecific() {
            return finSpecific;
        }

        public void setFinSpecific(String finSpecific) {
            this.finSpecific = finSpecific;
        }

        public String getLanguage() {
            return language;
        }

        public void setLanguage(String language) {
            this.language = language;
        }

        public String getPreviousStudyLevel() {
            return previousStudyLevel;
        }

        public void setPreviousStudyLevel(String previousStudyLevel) {
            this.previousStudyLevel = previousStudyLevel;
        }

        public String getEmail() {
            return email;
        }

        public void setEmail(String email) {
            this.email = email;
        }

        public Boolean getIsPeriod() {
            return isPeriod;
        }

        public void setIsPeriod(Boolean isPeriod) {
            this.isPeriod = isPeriod;
        }

        public Long getStudyPeriodStart() {
            return studyPeriodStart;
        }

        public void setStudyPeriodStart(Long studyPeriodStart) {
            this.studyPeriodStart = studyPeriodStart;
        }

        public Long getStudyPeriodEnd() {
            return studyPeriodEnd;
        }

        public void setStudyPeriodEnd(Long studyPeriodEnd) {
            this.studyPeriodEnd = studyPeriodEnd;
        }

        public LocalDate getNominalStudyEnd() {
            return nominalStudyEnd;
        }

        public void setNominalStudyEnd(LocalDate nominalStudyEnd) {
            this.nominalStudyEnd = nominalStudyEnd;
        }

        public Boolean getIsAbroad() {
            return isAbroad;
        }

        public void setIsAbroad(Boolean isAbroad) {
            this.isAbroad = isAbroad;
        }

        public String getEhisSchool() {
            return ehisSchool;
        }

        public void setEhisSchool(String ehisSchool) {
            this.ehisSchool = ehisSchool;
        }

        public String getAbroadSchool() {
            return abroadSchool;
        }

        public void setAbroadSchool(String abroadSchool) {
            this.abroadSchool = abroadSchool;
        }

        public String getCountry() {
            return country;
        }

        public void setCountry(String country) {
            this.country = country;
        }

        public String getAbroadPurpose() {
            return abroadPurpose;
        }

        public void setAbroadPurpose(String abroadPurpose) {
            this.abroadPurpose = abroadPurpose;
        }

        public String getAbroadProgramme() {
            return abroadProgramme;
        }

        public void setAbroadProgramme(String abroadProgramme) {
            this.abroadProgramme = abroadProgramme;
        }

        public BigDecimal getAmountPaid() {
            return amountPaid;
        }

        public void setAmountPaid(BigDecimal amountPaid) {
            this.amountPaid = amountPaid;
        }

        public String getBankAccount() {
            return bankAccount;
        }

        public void setBankAccount(String bankAccount) {
            this.bankAccount = bankAccount;
        }

        public Long getApplication() {
            return application;
        }

        public void setApplication(Long application) {
            this.application = application;
        }

        public Long getStudent() {
            return student;
        }

        public void setStudent(Long student) {
            this.student = student;
        }

        public Long getSaisApplication() {
            return saisApplication;
        }

        public void setSaisApplication(Long saisApplication) {
            this.saisApplication = saisApplication;
        }

        public Long getScholarshipApplication() {
            return scholarshipApplication;
        }

        public void setScholarshipApplication(Long scholarshipApplication) {
            this.scholarshipApplication = scholarshipApplication;
        }

        public String getAddInfo() {
            return addInfo;
        }

        public void setAddInfo(String addInfo) {
            this.addInfo = addInfo;
        }

        public Boolean getIsAbsence() {
            return isAbsence;
        }

        public void setIsAbsence(Boolean isAbsence) {
            this.isAbsence = isAbsence;
        }

        public List<String> getOccupations() {
            return occupations;
        }

        public void setOccupations(List<String> occupations) {
            this.occupations = occupations;
        }

        public Map<String, List<String>> getSpecialities() {
            return specialities;
        }

        public void setSpecialities(Map<String, List<String>> specialities) {
            this.specialities = specialities;
        }

        public List<DirectiveFormStudentModule> getModules() {
            return modules;
        }

        public void setModules(List<DirectiveFormStudentModule> modules) {
            this.modules = modules;
        }

        public List<ScholarshipApplicationSelectDto> getScholarshipApplications() {
            return scholarshipApplications;
        }

        public void setScholarshipApplications(List<ScholarshipApplicationSelectDto> scholarshipApplications) {
            this.scholarshipApplications = scholarshipApplications;
        }

        public List<ExistingDirectiveStudentDto> getExistingDirectiveStudents() {
            return existingDirectiveStudents;
        }

        public void setExistingDirectiveStudents(List<ExistingDirectiveStudentDto> existingDirectiveStudents) {
            this.existingDirectiveStudents = existingDirectiveStudents;
        }

        public Long getDirectiveStudent() {
            return directiveStudent;
        }

        public void setDirectiveStudent(Long directiveStudent) {
            this.directiveStudent = directiveStudent;
        }

        public String getDormitory() {
            return dormitory;
        }

        public void setDormitory(String dormitory) {
            this.dormitory = dormitory;
        }

        public Long getApelSchoolId() {
            return apelSchoolId;
        }

        public void setApelSchoolId(Long apelSchoolId) {
            this.apelSchoolId = apelSchoolId;
        }

        public Boolean getDiplomaChk() {
            return diplomaChk;
        }

        public void setDiplomaChk(Boolean diplomaChk) {
            this.diplomaChk = diplomaChk;
        }

        public Boolean getDiplomaSupplementChk() {
            return diplomaSupplementChk;
        }

        public void setDiplomaSupplementChk(Boolean diplomaSupplementChk) {
            this.diplomaSupplementChk = diplomaSupplementChk;
        }

        public Boolean getDiplomaSupplementEnChk() {
            return diplomaSupplementEnChk;
        }

        public void setDiplomaSupplementEnChk(Boolean diplomaSupplementEnChk) {
            this.diplomaSupplementEnChk = diplomaSupplementEnChk;
        }

    }

    public static class DirectiveFormStudentModule {
        private Long id;
        private AutocompleteResult module;
        @Required
        private Long curriculumVersionOmodule;
        @Required
        @Size(max = 4000, message = "maxlength")
        private String addInfo;

        public static DirectiveFormStudentModule of(DirectiveStudentModule directiveStudentModule) {
            DirectiveFormStudentModule dto = new DirectiveFormStudentModule();
            dto.setId(directiveStudentModule.getId());
            dto.setModule(AutocompleteResult.of(directiveStudentModule.getCurriculumVersionOmodule(), false));
            dto.setCurriculumVersionOmodule(EntityUtil.getId(directiveStudentModule.getCurriculumVersionOmodule()));
            dto.setAddInfo(directiveStudentModule.getAddInfo());
            return dto;
        }

        public Long getId() {
            return id;
        }

        public void setId(Long id) {
            this.id = id;
        }

        public AutocompleteResult getModule() {
            return module;
        }

        public void setModule(AutocompleteResult module) {
            this.module = module;
        }

        public Long getCurriculumVersionOmodule() {
            return curriculumVersionOmodule;
        }

        public void setCurriculumVersionOmodule(Long curriculumVersionOmodule) {
            this.curriculumVersionOmodule = curriculumVersionOmodule;
        }

        public String getAddInfo() {
            return addInfo;
        }

        public void setAddInfo(String addInfo) {
            this.addInfo = addInfo;
        }

    }
}
