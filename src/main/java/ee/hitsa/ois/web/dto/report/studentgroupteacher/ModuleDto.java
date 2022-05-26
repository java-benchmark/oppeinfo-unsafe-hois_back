package ee.hitsa.ois.web.dto.report.studentgroupteacher;

import java.util.ArrayList;
import java.util.List;

import ee.hitsa.ois.util.Translatable;
import ee.hitsa.ois.web.dto.AutocompleteResult;
import ee.hitsa.ois.web.dto.curriculum.CurriculumModuleOutcomeResult;

public class ModuleDto implements Translatable {

    private Long id;
    private String nameEt;
    private String nameEn;
    private String type;
    private List<AutocompleteResult> journals = new ArrayList<>();
    private Boolean isPracticeModule;
    private Boolean isPracticeModuleGraded;
    private List<AutocompleteResult> practiceModuleThemes = new ArrayList<>();
    private List<CurriculumModuleOutcomeResult> outcomes = new ArrayList<>();
    private Long colspan = 0L;

    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    @Override
    public String getNameEt() {
        return nameEt;
    }

    public void setNameEt(String nameEt) {
        this.nameEt = nameEt;
    }

    @Override
    public String getNameEn() {
        return nameEn;
    }

    public void setNameEn(String nameEn) {
        this.nameEn = nameEn;
    }

    public String getType() {
        return type;
    }

    public void setType(String type) {
        this.type = type;
    }

    public List<AutocompleteResult> getJournals() {
        return journals;
    }

    public void setJournals(List<AutocompleteResult> journals) {
        this.journals = journals;
    }

    public Boolean getIsPracticeModule() {
        return isPracticeModule;
    }

    public void setIsPracticeModule(Boolean isPracticeModule) {
        this.isPracticeModule = isPracticeModule;
    }

    public Boolean getIsPracticeModuleGraded() {
        return isPracticeModuleGraded;
    }

    public void setIsPracticeModuleGraded(Boolean isPracticeModuleGraded) {
        this.isPracticeModuleGraded = isPracticeModuleGraded;
    }

    public List<AutocompleteResult> getPracticeModuleThemes() {
        return practiceModuleThemes;
    }

    public void setPracticeModuleThemes(List<AutocompleteResult> practiceModuleThemes) {
        this.practiceModuleThemes = practiceModuleThemes;
    }

    public List<CurriculumModuleOutcomeResult> getOutcomes() {
        return outcomes;
    }

    public void setOutcomes(List<CurriculumModuleOutcomeResult> outcomes) {
        this.outcomes = outcomes;
    }

    public Long getColspan() {
        return colspan;
    }

    public void setColspan(Long colspan) {
        this.colspan = colspan;
    }
    
    public List<Long> getExcelCopsanColums() {
        List<Long> columns = new ArrayList<>();
        for (int i = 0; i < this.colspan.longValue() - 1; i++) {
            columns.add(Long.valueOf(i));
        }
        return columns;
    }

}
