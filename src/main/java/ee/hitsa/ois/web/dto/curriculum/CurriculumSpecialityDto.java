package ee.hitsa.ois.web.dto.curriculum;

import java.math.BigDecimal;

import javax.validation.constraints.Max;
import javax.validation.constraints.Min;
import javax.validation.constraints.NotNull;
import javax.validation.constraints.Size;

import org.hibernate.validator.constraints.NotBlank;

import ee.hitsa.ois.domain.curriculum.CurriculumSpeciality;
import ee.hitsa.ois.enums.MainClassCode;
import ee.hitsa.ois.util.EntityUtil;
import ee.hitsa.ois.validation.ClassifierRestriction;
import ee.hitsa.ois.web.commandobject.VersionedCommand;

public class CurriculumSpecialityDto extends VersionedCommand {
    
    private Long id;
    private Long referenceNumber;
    @NotBlank
    @Size(max=100)
    private String nameEt;
    @NotBlank
    @Size(max=100)
    private String nameEn;
    @NotNull
    @Min(0)
    @Max(999)
    private BigDecimal credits;
    @Size(max=1000)
    private String description;
    @Size(max=255)
    private String occupationEt;
    @Size(max=255)
    private String occupationEn;
    @ClassifierRestriction(MainClassCode.KUTSE)
    private String occupation;
    private boolean addedToVersion;

    public static CurriculumSpecialityDto of(CurriculumSpeciality speciality) {
        CurriculumSpecialityDto dto = EntityUtil.bindToDto(speciality, new CurriculumSpecialityDto(), "referenceNumber", "curriculum");
        dto.setAddedToVersion(speciality.isAddedToVersion());
        return dto;
    }
    
    public Long getReferenceNumber() {
        return referenceNumber;
    }

    public void setReferenceNumber(Long referenceNumber) {
        this.referenceNumber = referenceNumber;
    }
    
    public boolean isAddedToVersion() {
        return addedToVersion;
    }
    public void setAddedToVersion(boolean addedToVersion) {
        this.addedToVersion = addedToVersion;
    }
    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
        setReferenceNumber(id);
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

    public BigDecimal getCredits() {
        return credits;
    }

    public void setCredits(BigDecimal credits) {
        this.credits = credits;
    }

    public String getDescription() {
        return description;
    }

    public void setDescription(String description) {
        this.description = description;
    }

    public String getOccupationEt() {
        return occupationEt;
    }

    public void setOccupationEt(String occupationEt) {
        this.occupationEt = occupationEt;
    }

    public String getOccupationEn() {
        return occupationEn;
    }

    public void setOccupationEn(String occupationEn) {
        this.occupationEn = occupationEn;
    }

    public String getOccupation() {
        return occupation;
    }

    public void setOccupation(String occupation) {
        this.occupation = occupation;
    }
}
