package ee.hitsa.ois.domain.sais;

import java.time.LocalDate;
import java.time.LocalDateTime;

import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;

import ee.hitsa.ois.domain.BaseEntityWithId;
import ee.hitsa.ois.domain.school.School;

@Entity
public class WsSaisLog extends BaseEntityWithId {

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(updatable = false)
    private School school;
    private String wsName;
    private LocalDate queryDateFrom;
    private LocalDate queryDateThru;
    private LocalDateTime processQueryStart;
    private LocalDateTime processQueryEnd;
    private String request;
    private String response;
    private Boolean hasXteeErrors;
    private Boolean hasOtherErrors;
    private Long recordCount;
    @ManyToOne
    private WsSaisLog firstWsSaisLog;

    public WsSaisLog() {
        this.hasOtherErrors = Boolean.FALSE;
        this.hasXteeErrors = Boolean.FALSE;
    }

    public School getSchool() {
        return school;
    }

    public void setSchool(School school) {
        this.school = school;
    }

    public String getWsName() {
        return wsName;
    }

    public void setWsName(String wsName) {
        this.wsName = wsName;
    }

    public LocalDate getQueryDateFrom() {
        return queryDateFrom;
    }

    public void setQueryDateFrom(LocalDate queryDateFrom) {
        this.queryDateFrom = queryDateFrom;
    }

    public LocalDate getQueryDateThru() {
        return queryDateThru;
    }

    public void setQueryDateThru(LocalDate queryDateThru) {
        this.queryDateThru = queryDateThru;
    }

    public LocalDateTime getProcessQueryStart() {
        return processQueryStart;
    }

    public void setProcessQueryStart(LocalDateTime processQueryStart) {
        this.processQueryStart = processQueryStart;
    }

    public LocalDateTime getProcessQueryEnd() {
        return processQueryEnd;
    }

    public void setProcessQueryEnd(LocalDateTime processQueryEnd) {
        this.processQueryEnd = processQueryEnd;
    }

    public String getRequest() {
        return request;
    }

    public void setRequest(String request) {
        this.request = request;
    }

    public String getResponse() {
        return response;
    }

    public void setResponse(String response) {
        this.response = response;
    }

    public Boolean getHasXteeErrors() {
        return hasXteeErrors;
    }

    public void setHasXteeErrors(Boolean hasXteeErrors) {
        this.hasXteeErrors = hasXteeErrors;
    }

    public Boolean getHasOtherErrors() {
        return hasOtherErrors;
    }

    public void setHasOtherErrors(Boolean hasOtherErrors) {
        this.hasOtherErrors = hasOtherErrors;
    }

    public Long getRecordCount() {
        return recordCount;
    }

    public void setRecordCount(Long recordCount) {
        this.recordCount = recordCount;
    }

    public WsSaisLog getFirstWsSaisLog() {
        return firstWsSaisLog;
    }

    public void setFirstWsSaisLog(WsSaisLog firstWsSaisLog) {
        this.firstWsSaisLog = firstWsSaisLog;
    }
}
