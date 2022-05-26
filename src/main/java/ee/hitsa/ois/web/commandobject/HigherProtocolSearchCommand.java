package ee.hitsa.ois.web.commandobject;

import java.time.LocalDate;

import ee.hitsa.ois.enums.MainClassCode;
import ee.hitsa.ois.validation.ClassifierRestriction;
import ee.hitsa.ois.validation.DateRange;

@DateRange(from = "confirmDateFrom", thru = "confirmDateThru")
@DateRange(from = "insertedFrom", thru = "insertedThru")
public class HigherProtocolSearchCommand {
    
    private Long studyPeriod;
    private Long teacher;
    private String subjectModule;
    @ClassifierRestriction(MainClassCode.PROTOKOLL_STAATUS)
    private String status;
    private String protocolNr;
    private LocalDate confirmDateFrom;
    private LocalDate confirmDateThru;
    private LocalDate insertedFrom;
    private LocalDate insertedThru;
    private Boolean showOnlyModuleProtocols;
    private EntityConnectionCommand studentGroup;

    public Long getStudyPeriod() {
        return studyPeriod;
    }

    public void setStudyPeriod(Long studyPeriod) {
        this.studyPeriod = studyPeriod;
    }

    public Long getTeacher() {
        return teacher;
    }

    public void setTeacher(Long teacher) {
        this.teacher = teacher;
    }

    public String getSubjectModule() {
        return subjectModule;
    }

    public void setSubjectModule(String subjectModule) {
        this.subjectModule = subjectModule;
    }

    public String getStatus() {
        return status;
    }

    public void setStatus(String status) {
        this.status = status;
    }

    public String getProtocolNr() {
        return protocolNr;
    }

    public void setProtocolNr(String protocolNr) {
        this.protocolNr = protocolNr;
    }

    public LocalDate getConfirmDateFrom() {
        return confirmDateFrom;
    }

    public void setConfirmDateFrom(LocalDate confirmDateFrom) {
        this.confirmDateFrom = confirmDateFrom;
    }

    public LocalDate getConfirmDateThru() {
        return confirmDateThru;
    }

    public void setConfirmDateThru(LocalDate confirmDateThru) {
        this.confirmDateThru = confirmDateThru;
    }

    public LocalDate getInsertedFrom() {
        return insertedFrom;
    }

    public void setInsertedFrom(LocalDate insertedFrom) {
        this.insertedFrom = insertedFrom;
    }

    public LocalDate getInsertedThru() {
        return insertedThru;
    }

    public void setInsertedThru(LocalDate insertedThru) {
        this.insertedThru = insertedThru;
    }

    public Boolean getShowOnlyModuleProtocols() {
        return showOnlyModuleProtocols;
    }

    public void setShowOnlyModuleProtocols(Boolean showOnlyModuleProtocols) {
        this.showOnlyModuleProtocols = showOnlyModuleProtocols;
    }

    public EntityConnectionCommand getStudentGroup() {
        return studentGroup;
    }

    public void setStudentGroup(EntityConnectionCommand studentGroup) {
        this.studentGroup = studentGroup;
    }
}
