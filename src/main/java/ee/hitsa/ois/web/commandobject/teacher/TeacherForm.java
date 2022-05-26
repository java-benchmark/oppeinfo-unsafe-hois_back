package ee.hitsa.ois.web.commandobject.teacher;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.Set;

import javax.validation.Valid;
import javax.validation.constraints.DecimalMin;
import javax.validation.constraints.Max;
import javax.validation.constraints.Min;
import javax.validation.constraints.NotNull;
import javax.validation.constraints.Size;

import ee.hitsa.ois.enums.MainClassCode;
import ee.hitsa.ois.validation.ClassifierRestriction;
import ee.hitsa.ois.validation.DateRange;
import ee.hitsa.ois.validation.EstonianIdCode;
import ee.hitsa.ois.validation.Required;
import ee.hitsa.ois.web.commandobject.VersionedCommand;
import ee.hitsa.ois.web.dto.AutocompleteResult;

public class TeacherForm extends VersionedCommand {

    @Valid
    @NotNull
    private TeacherPersonForm person;
    @Valid
    private Set<TeacherPositionEhisForm> teacherPositionEhis;
    private Boolean isVocational = Boolean.FALSE;
    private Boolean isHigher = Boolean.FALSE;
    private Boolean isActive = Boolean.TRUE;
    private AutocompleteResult teacherOccupation;
    private Boolean isStudyPeriodScheduleLoad = Boolean.FALSE;
    @Required
    @Size(max = 100)
    private String email;
    @Size(max = 100)
    private String phone;
    @NotNull
    @Min(0)
    @Max(9999)
    private Short scheduleLoad;
    @Size(max = 20)
    private String rtipNr;
    @Size(max = 1000)
    private String addInfo;
    @ClassifierRestriction(MainClassCode.EHIS_EMAKEEL)
    private String nativeLanguage;
    private LocalDateTime ehisLastSuccessfulDate;
    private String untisCode;

    public Boolean getIsStudyPeriodScheduleLoad() {
        return isStudyPeriodScheduleLoad;
    }

    public void setIsStudyPeriodScheduleLoad(Boolean studyPeriodScheduleLoad) {
        isStudyPeriodScheduleLoad = studyPeriodScheduleLoad;
    }

    public Boolean getIsActive() {
        return isActive;
    }

    public void setIsActive(Boolean isActive) {
        this.isActive = isActive;
    }

    public TeacherPersonForm getPerson() {
        return person;
    }

    public void setPerson(TeacherPersonForm person) {
        this.person = person;
    }

    public Boolean getIsVocational() {
        return isVocational;
    }

    public void setIsVocational(Boolean vocational) {
        isVocational = vocational;
    }

    public Boolean getIsHigher() {
        return isHigher;
    }

    public void setIsHigher(Boolean higher) {
        isHigher = higher;
    }

    public AutocompleteResult getTeacherOccupation() {
        return teacherOccupation;
    }

    public void setTeacherOccupation(AutocompleteResult teacherOccupation) {
        this.teacherOccupation = teacherOccupation;
    }

    public Set<TeacherPositionEhisForm> getTeacherPositionEhis() {
        return teacherPositionEhis;
    }

    public void setTeacherPositionEhis(Set<TeacherPositionEhisForm> teacherPositionEhis) {
        this.teacherPositionEhis = teacherPositionEhis;
    }

    public Short getScheduleLoad() {
        return scheduleLoad;
    }

    public void setScheduleLoad(Short scheduleLoad) {
        this.scheduleLoad = scheduleLoad;
    }

    public String getEmail() {
        return email;
    }

    public void setEmail(String email) {
        this.email = email;
    }

    public String getPhone() {
        return phone;
    }

    public void setPhone(String phone) {
        this.phone = phone;
    }

    public String getRtipNr() {
        return rtipNr;
    }

    public void setRtipNr(String rtipNr) {
        this.rtipNr = rtipNr;
    }

    public String getAddInfo() {
        return addInfo;
    }

    public void setAddInfo(String addInfo) {
        this.addInfo = addInfo;
    }
    
    public String getNativeLanguage() {
        return nativeLanguage;
    }

    public void setNativeLanguage(String nativeLanguage) {
        this.nativeLanguage = nativeLanguage;
    }

    public LocalDateTime getEhisLastSuccessfulDate() {
        return this.ehisLastSuccessfulDate;
    }
    
    public void setEhisLastSuccessfulDate(LocalDateTime ehisLastSuccessfulDate) {
        this.ehisLastSuccessfulDate = ehisLastSuccessfulDate;
    }

    public String getUntisCode() {
		return untisCode;
	}

	public void setUntisCode(String untisCode) {
		this.untisCode = untisCode;
	}

	public static class TeacherPersonForm {

        private Long id;
        @EstonianIdCode
        private String idcode;
        @Required
        @Size(max = 100)
        private String firstname;
        @Required
        @Size(max = 100)
        private String lastname;
        @Required
        @ClassifierRestriction(MainClassCode.RIIK)
        private String citizenship;
        @Size(max = 100)
        private String nativeLanguage;
        private LocalDate birthdate;
        @Required
        @ClassifierRestriction(MainClassCode.SUGU)
        private String sex;
        @Size(max = 100)
        private String email;
        @Size(max = 100)
        private String phone;

        public String getIdcode() {
            return idcode;
        }

        public void setIdcode(String idcode) {
            this.idcode = idcode;
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

        public String getCitizenship() {
            return citizenship;
        }

        public void setCitizenship(String citizenship) {
            this.citizenship = citizenship;
        }

        public String getNativeLanguage() {
            return nativeLanguage;
        }

        public void setNativeLanguage(String nativeLanguage) {
            this.nativeLanguage = nativeLanguage;
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

        public String getEmail() {
            return email;
        }

        public void setEmail(String email) {
            this.email = email;
        }

        public String getPhone() {
            return phone;
        }

        public void setPhone(String phone) {
            this.phone = phone;
        }

        public Long getId() {
            return id;
        }

        public void setId(Long id) {
            this.id = id;
        }
    }

    @DateRange(from = "contractStart", thru = "contractEnd")
    public static class TeacherPositionEhisForm {

        private Long id;
        private Boolean isVocational = Boolean.FALSE;
        @Required
        @ClassifierRestriction(MainClassCode.EHIS_AMETIKOHT)
        private String position;
        @Size(max = 255)
        private String positionSpecificationEt;
        @Size(max = 255)
        private String positionSpecificationEn;
        @Required
        @ClassifierRestriction(MainClassCode.EHIS_LEPING)
        private String contractType;
        @NotNull
        private LocalDate contractStart;
        private LocalDate contractEnd;
        private Boolean isContractEnded = Boolean.FALSE;
        private Boolean isTeacher = Boolean.FALSE;
        private Boolean meetsQualification = Boolean.FALSE;
        private Boolean isChildCare = Boolean.FALSE;
        private Boolean isClassTeacher = Boolean.FALSE;
        @NotNull
        @DecimalMin("0.0")
        private BigDecimal load;

        // TODO: VALIDATION(this or opetaja keel req)
        @ClassifierRestriction(MainClassCode.EHIS_TOOSUHE)
        private String employmentType;
        @Size(max = 255)
        private String employmentTypeSpecification;
        @Size(max = 100)
        private String employmentCode;

        // TODO: VALIDATION(this or toosuhe req)
        @ClassifierRestriction(MainClassCode.EHIS_OPETAJA_KEEL)
        private String language;
        private Long schoolDepartment;

        public Boolean getIsVocational() {
            return isVocational;
        }

        public void setIsVocational(Boolean vocational) {
            isVocational = vocational;
        }

        public String getPosition() {
            return position;
        }

        public void setPosition(String position) {
            this.position = position;
        }

        public String getPositionSpecificationEt() {
            return positionSpecificationEt;
        }

        public void setPositionSpecificationEt(String positionSpecificationEt) {
            this.positionSpecificationEt = positionSpecificationEt;
        }

        public String getContractType() {
            return contractType;
        }

        public void setContractType(String contractType) {
            this.contractType = contractType;
        }

        public LocalDate getContractStart() {
            return contractStart;
        }

        public void setContractStart(LocalDate contractStart) {
            this.contractStart = contractStart;
        }

        public LocalDate getContractEnd() {
            return contractEnd;
        }

        public void setContractEnd(LocalDate contractEnd) {
            this.contractEnd = contractEnd;
        }

        public Boolean getIsContractEnded() {
            return isContractEnded;
        }

        public void setIsContractEnded(Boolean contractEnded) {
            isContractEnded = contractEnded;
        }

        public BigDecimal getLoad() {
            return load;
        }

        public void setLoad(BigDecimal load) {
            this.load = load;
        }

        public Long getId() {
            return id;
        }

        public void setId(Long id) {
            this.id = id;
        }

        public Boolean getIsTeacher() {
            return isTeacher;
        }

        public void setIsTeacher(Boolean teacher) {
            isTeacher = teacher;
        }

        public String getLanguage() {
            return language;
        }

        public void setLanguage(String language) {
            this.language = language;
        }

        public Boolean getIsClassTeacher() {
            return isClassTeacher;
        }

        public void setIsClassTeacher(Boolean classTeacher) {
            isClassTeacher = classTeacher;
        }

        public Boolean getIsChildCare() {
            return isChildCare;
        }

        public void setIsChildCare(Boolean childCare) {
            isChildCare = childCare;
        }

        public Boolean getMeetsQualification() {
            return meetsQualification;
        }

        public void setMeetsQualification(Boolean meetsQualification) {
            this.meetsQualification = meetsQualification;
        }

        public String getEmploymentType() {
            return employmentType;
        }

        public void setEmploymentType(String employmentType) {
            this.employmentType = employmentType;
        }

        public String getEmploymentTypeSpecification() {
            return employmentTypeSpecification;
        }

        public void setEmploymentTypeSpecification(String employmentTypeSpecification) {
            this.employmentTypeSpecification = employmentTypeSpecification;
        }

        public String getPositionSpecificationEn() {
            return positionSpecificationEn;
        }

        public void setPositionSpecificationEn(String positionSpecificationEn) {
            this.positionSpecificationEn = positionSpecificationEn;
        }

        public String getEmploymentCode() {
            return employmentCode;
        }

        public void setEmploymentCode(String employmentCode) {
            this.employmentCode = employmentCode;
        }

        public Long getSchoolDepartment() {
            return schoolDepartment;
        }

        public void setSchoolDepartment(Long schoolDepartment) {
            this.schoolDepartment = schoolDepartment;
        }
    }
}
