package ee.hitsa.ois.web.dto.timetable;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import ee.hitsa.ois.web.dto.AutocompleteResult;

public class StudentJournalDto {
    private Long id;
    private String nameEt;
    private Long studyYearId;
    private String yearCode;
    private Map<String, Long> absences;
    private String teachers;
    private AutocompleteResult modules;
    private List<StudentJournalEntryDto> journalEntries = new ArrayList<>();
    
    public Long getId() {
        return id;
    }
    
    public void setId(Long id) {
        this.id = id;
    }
    
    public String getNameEt() {
        return nameEt;
    }
    
    public void setNameEt(String nameEt) {
        this.nameEt = nameEt;
    }
    
    public Long getStudyYearId() {
        return studyYearId;
    }
    
    public void setStudyYearId(Long studyYearId) {
        this.studyYearId = studyYearId;
    }
    
    public String getYearCode() {
        return yearCode;
    }
    
    public void setYearCode(String yearCode) {
        this.yearCode = yearCode;
    }

    public Map<String, Long> getAbsences() {
        return absences;
    }

    public void setAbsences(Map<String, Long> absences) {
        this.absences = absences;
    }

    public String getTeachers() {
        return teachers;
    }
    
    public void setTeachers(String teachers) {
        this.teachers = teachers;
    }
    
    public AutocompleteResult getModules() {
        return modules;
    }
    
    public void setModules(AutocompleteResult modules) {
        this.modules = modules;
    }
    
    public List<StudentJournalEntryDto> getJournalEntries() {
        return journalEntries;
    }
    
    public void setJournalEntries(List<StudentJournalEntryDto> journalEntries) {
        this.journalEntries = journalEntries;
    }

}
