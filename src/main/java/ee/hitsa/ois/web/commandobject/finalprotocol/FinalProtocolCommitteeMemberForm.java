package ee.hitsa.ois.web.commandobject.finalprotocol;

public class FinalProtocolCommitteeMemberForm {

    private Long id;
    private Long committeeMemberId;

    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public Long getCommitteeMemberId() {
        return committeeMemberId;
    }

    public void setCommitteeMemberId(Long committeeMemberId) {
        this.committeeMemberId = committeeMemberId;
    }
    
}
