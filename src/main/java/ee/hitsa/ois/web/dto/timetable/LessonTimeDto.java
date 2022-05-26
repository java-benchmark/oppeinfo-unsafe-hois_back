package ee.hitsa.ois.web.dto.timetable;

import java.time.LocalTime;
import java.util.ArrayList;
import java.util.List;

import javax.validation.constraints.Min;
import javax.validation.constraints.NotNull;

import ee.hitsa.ois.domain.timetable.LessonTime;
import ee.hitsa.ois.util.EntityUtil;
import ee.hitsa.ois.validation.TimeRange;

@TimeRange(from = "startTime", thru = "endTime")
public class LessonTimeDto {

    private Long id;
    @NotNull
    private LocalTime startTime;
    @NotNull
    private LocalTime endTime;
    @NotNull
    @Min(1)
    private Short lessonNr;
    private Boolean dayMon;
    private Boolean dayTue;
    private Boolean dayWed;
    private Boolean dayThu;
    private Boolean dayFri;
    private Boolean daySat;
    private Boolean daySun;
    private Long buildingId;
    private List<Long> buildingIds = new ArrayList<>();

    public static LessonTimeDto of(LessonTime lessonTime) {
        LessonTimeDto dto = EntityUtil.bindToDto(lessonTime, new LessonTimeDto());
        return dto;
    }

    public LessonTimeDto() {

    }

    public LessonTimeDto(Long id, LocalTime startTime, LocalTime endTime, short lessonNr, Boolean dayMon,
            Boolean dayTue, Boolean dayWed, Boolean dayThu, Boolean dayFri, Boolean daySat, Boolean daySun,
            List<Long> buildingIds) {
        this.id = id;
        this.startTime = startTime;
        this.endTime = endTime;
        this.lessonNr = Short.valueOf(lessonNr);
        this.dayMon = dayMon;
        this.dayTue = dayTue;
        this.dayWed = dayWed;
        this.dayThu = dayThu;
        this.dayFri = dayFri;
        this.daySat = daySat;
        this.daySun = daySun;
        this.buildingIds = buildingIds;
    }

    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public LocalTime getStartTime() {
        return startTime;
    }

    public void setStartTime(LocalTime startTime) {
        this.startTime = startTime;
    }

    public LocalTime getEndTime() {
        return endTime;
    }

    public void setEndTime(LocalTime endTime) {
        this.endTime = endTime;
    }

    public Short getLessonNr() {
        return lessonNr;
    }

    public void setLessonNr(Short lessonNr) {
        this.lessonNr = lessonNr;
    }

    public Boolean getDayMon() {
        return dayMon;
    }

    public void setDayMon(Boolean dayMon) {
        this.dayMon = dayMon;
    }

    public Boolean getDayTue() {
        return dayTue;
    }

    public void setDayTue(Boolean dayTue) {
        this.dayTue = dayTue;
    }

    public Boolean getDayWed() {
        return dayWed;
    }

    public void setDayWed(Boolean dayWed) {
        this.dayWed = dayWed;
    }

    public Boolean getDayThu() {
        return dayThu;
    }

    public void setDayThu(Boolean dayThu) {
        this.dayThu = dayThu;
    }

    public Boolean getDayFri() {
        return dayFri;
    }

    public void setDayFri(Boolean dayFri) {
        this.dayFri = dayFri;
    }

    public Boolean getDaySat() {
        return daySat;
    }

    public void setDaySat(Boolean daySat) {
        this.daySat = daySat;
    }

    public Boolean getDaySun() {
        return daySun;
    }

    public void setDaySun(Boolean daySun) {
        this.daySun = daySun;
    }

    public Long getBuildingId() {
        return buildingId;
    }

    public void setBuildingId(Long buildingId) {
        this.buildingId = buildingId;
    }

    public List<Long> getBuildingIds() {
        return buildingIds;
    }

    public void setBuildingIds(List<Long> buildingIds) {
        this.buildingIds = buildingIds;
    }

}
