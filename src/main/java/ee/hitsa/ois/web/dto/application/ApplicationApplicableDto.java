package ee.hitsa.ois.web.dto.application;

public class ApplicationApplicableDto {

    private static final ApplicationApplicableDto TRUE_VALUE = new ApplicationApplicableDto();

    private Boolean isAllowed;
    private String reason;

    public ApplicationApplicableDto() {
        this.isAllowed = Boolean.TRUE;
    }

    public ApplicationApplicableDto(String reason) {
        this.isAllowed = Boolean.FALSE;
        this.reason = reason;
    }
    public Boolean getIsAllowed() {
        return isAllowed;
    }
    public void setIsAllowed(Boolean isAllowed) {
        this.isAllowed = isAllowed;
    }
    public String getReason() {
        return reason;
    }
    public void setReason(String reason) {
        this.reason = reason;
    }

    public static ApplicationApplicableDto trueValue() {
        return TRUE_VALUE;
    }

}
