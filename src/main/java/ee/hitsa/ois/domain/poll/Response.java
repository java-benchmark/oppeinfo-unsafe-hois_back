package ee.hitsa.ois.domain.poll;

import java.util.ArrayList;
import java.util.List;

import javax.persistence.CascadeType;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.OneToMany;
import javax.persistence.OneToOne;

import ee.hitsa.ois.domain.BaseEntityWithId;
import ee.hitsa.ois.domain.Classifier;

@Entity
public class Response extends BaseEntityWithId {
    
    @ManyToOne(optional = false, fetch = FetchType.LAZY)
    private Poll poll;
    @ManyToOne(optional = false, fetch = FetchType.LAZY)
    private Classifier status;
    @OneToOne(mappedBy="response")
    private ResponseObject responseObject;
    @OneToMany(cascade = CascadeType.ALL, orphanRemoval = true)
    @JoinColumn(name = "response_id", nullable = false, updatable = false, insertable = false)
    private List<ResponseQuestionAnswer> responseQuestionAnswers = new ArrayList<>();
    @OneToMany(cascade = CascadeType.ALL, orphanRemoval = true)
    @JoinColumn(name = "response_id", nullable = false, updatable = false, insertable = false)
    private List<ResponseSubject> responseSubjects = new ArrayList<>();
    
    public Poll getPoll() {
        return poll;
    }
    public void setPoll(Poll poll) {
        this.poll = poll;
    }
    public Classifier getStatus() {
        return status;
    }
    public void setStatus(Classifier status) {
        this.status = status;
    }
    public ResponseObject getResponseObject() {
        return responseObject;
    }
    public void setResponseObject(ResponseObject responseObject) {
        this.responseObject = responseObject;
    }
    public List<ResponseQuestionAnswer> getResponseQuestionAnswers() {
        return responseQuestionAnswers;
    }
    public void setResponseQuestionAnswers(List<ResponseQuestionAnswer> responseQuestionAnswers) {
        this.responseQuestionAnswers = responseQuestionAnswers;
    }
    public List<ResponseSubject> getResponseSubjects() {
        return responseSubjects;
    }
    public void setResponseSubjects(List<ResponseSubject> responseSubjects) {
        this.responseSubjects = responseSubjects;
    }
}
    
