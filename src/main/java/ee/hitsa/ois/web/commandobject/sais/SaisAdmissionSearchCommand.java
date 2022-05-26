package ee.hitsa.ois.web.commandobject.sais;

import ee.hitsa.ois.enums.MainClassCode;
import ee.hitsa.ois.validation.ClassifierRestriction;

public class SaisAdmissionSearchCommand {

    private String code;
    private Long curriculumVersion;
    @ClassifierRestriction(MainClassCode.OPPEVORM)
    private String studyForm;
    @ClassifierRestriction(MainClassCode.FINALLIKAS)
    private String fin;
    private Boolean is_archived;
    
    public void setArchived(Boolean archived) {
    	this.is_archived = archived;
    }
    
    public Boolean getArchived() {
    	return is_archived;
    }

    public String getCode() {
        return code;
    }

    public void setCode(String code) {
        this.code = code;
    }

    public Long getCurriculumVersion() {
        return curriculumVersion;
    }

    public void setCurriculumVersion(Long curriculumVersion) {
        this.curriculumVersion = curriculumVersion;
    }

    public String getStudyForm() {
        return studyForm;
    }

    public void setStudyForm(String studyForm) {
        this.studyForm = studyForm;
    }

    public String getFin() {
        return fin;
    }

    public void setFin(String fin) {
        this.fin = fin;
    }
}
