package ee.hitsa.ois.web.commandobject.scholarship;

import java.util.List;

import javax.validation.Valid;

import ee.hitsa.ois.validation.Required;

public class ScholarshipApplicationListSubmitForm {

    @Required
    @Valid
    private List<ScholarshiApplicationRejectionForm> applications;

    public List<ScholarshiApplicationRejectionForm> getApplications() {
        return applications;
    }

    public void setApplications(List<ScholarshiApplicationRejectionForm> applications) {
        this.applications = applications;
    }
}
