package ee.hitsa.ois.web.dto.poll;

import java.time.LocalDate;
import java.util.List;
import java.util.Set;

import ee.hitsa.ois.web.dto.AutocompleteResult;
import ee.hitsa.ois.web.dto.LiteralResult;

public class ThemesDto {
    
    private String type;
    private String nameEt;
    private Boolean isThemePageable;
    private Boolean confirmed;
    private String foreword;
    private String afterword;
    private List<ThemeDto> themes;
    private Long responseId;
    private Set<LiteralResult> subjects;
    private List<LiteralResult> journals;
    private LocalDate startDate;
    private LocalDate endDate;
    private AutocompleteResult name;
    private String status;
    
    public ThemesDto(List<ThemeDto> themes, Boolean confirmed, String foreword, String afterword) {
        this.themes = themes;
        this.confirmed = confirmed;
        this.foreword = foreword;
        this.afterword = afterword;
    }
    
    public ThemesDto() {}
    
    public List<ThemeDto> getThemes() {
        return themes;
    }

    public void setThemes(List<ThemeDto> themes) {
        this.themes = themes;
    }

    public Boolean getConfirmed() {
        return confirmed;
    }

    public void setConfirmed(Boolean confirmed) {
        this.confirmed = confirmed;
    }

    public String getForeword() {
        return foreword;
    }

    public void setForeword(String foreword) {
        this.foreword = foreword;
    }

    public String getAfterword() {
        return afterword;
    }

    public void setAfterword(String afterword) {
        this.afterword = afterword;
    }

    public void setResponseId(Long responseId) {
        this.responseId = responseId;
    }

    public Long getResponseId() {
        return responseId;
    }

    public Boolean getIsThemePageable() {
        return isThemePageable;
    }

    public void setIsThemePageable(Boolean isThemePageable) {
        this.isThemePageable = isThemePageable;
    }

    public String getType() {
        return type;
    }

    public void setType(String type) {
        this.type = type;
    }

    public Set<LiteralResult> getSubjects() {
        return subjects;
    }

    public void setSubjects(Set<LiteralResult> subjects) {
        this.subjects = subjects;
    }

    public List<LiteralResult> getJournals() {
        return journals;
    }

    public void setJournals(List<LiteralResult> journals) {
        this.journals = journals;
    }

    public String getNameEt() {
        return nameEt;
    }

    public void setNameEt(String nameEt) {
        this.nameEt = nameEt;
    }

    public LocalDate getStartDate() {
        return startDate;
    }

    public void setStartDate(LocalDate startDate) {
        this.startDate = startDate;
    }

    public LocalDate getEndDate() {
        return endDate;
    }

    public void setEndDate(LocalDate endDate) {
        this.endDate = endDate;
    }

    public AutocompleteResult getName() {
        return name;
    }

    public void setName(AutocompleteResult name) {
        this.name = name;
    }

    public String getStatus() {
        return status;
    }

    public void setStatus(String status) {
        this.status = status;
    }
}
