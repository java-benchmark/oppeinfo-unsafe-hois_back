package ee.hitsa.ois.web.commandobject.studymaterial;

public class JournalSearchCommand {

    private Long teacher;
    private Long studyYear;
    private Long studentGroup;
    private Long journal;
    
    public Long getTeacher() {
        return teacher;
    }
    public void setTeacher(Long teacher) {
        this.teacher = teacher;
    }
    
    public Long getStudyYear() {
        return studyYear;
    }
    public void setStudyYear(Long studyYear) {
        this.studyYear = studyYear;
    }
    
    public Long getStudentGroup() {
        return studentGroup;
    }
    public void setStudentGroup(Long studentGroup) {
        this.studentGroup = studentGroup;
    }
    public Long getJournal() {
        return journal;
    }
    public void setJournal(Long journal) {
        this.journal = journal;
    }
    
}
