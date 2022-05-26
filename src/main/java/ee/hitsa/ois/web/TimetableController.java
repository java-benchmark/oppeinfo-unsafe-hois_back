package ee.hitsa.ois.web;

import java.io.IOException;
import java.time.LocalDate;
import java.util.List;
import java.util.Map;

import javax.persistence.EntityManager;
import javax.servlet.http.HttpServletResponse;
import javax.validation.Valid;
import javax.xml.bind.JAXBException;

import ee.hitsa.ois.domain.school.School;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import ee.hitsa.ois.domain.StudyPeriod;
import ee.hitsa.ois.domain.StudyYear;
import ee.hitsa.ois.domain.student.Student;
import ee.hitsa.ois.domain.timetable.Timetable;
import ee.hitsa.ois.enums.SchoolTimetableType;
import ee.hitsa.ois.exception.HoisException;
import ee.hitsa.ois.service.TimetableGenerationService;
import ee.hitsa.ois.service.TimetableService;
import ee.hitsa.ois.service.XmlService;
import ee.hitsa.ois.service.security.HoisUserDetails;
import ee.hitsa.ois.util.ClassifierUtil;
import ee.hitsa.ois.util.EnumUtil;
import ee.hitsa.ois.util.HttpUtil;
import ee.hitsa.ois.util.TimetableUserRights;
import ee.hitsa.ois.util.UserUtil;
import ee.hitsa.ois.util.WithEntity;
import ee.hitsa.ois.web.commandobject.TimetableCopyForm;
import ee.hitsa.ois.web.commandobject.TimetableRoomAndTimeForm;
import ee.hitsa.ois.web.commandobject.timetable.TimetableEditForm;
import ee.hitsa.ois.web.commandobject.timetable.TimetableEventHigherForm;
import ee.hitsa.ois.web.commandobject.timetable.TimetableEventVocationalForm;
import ee.hitsa.ois.web.commandobject.timetable.TimetableManagementSearchCommand;
import ee.hitsa.ois.web.dto.StudyYearSearchDto;
import ee.hitsa.ois.web.dto.TimetableImportErrorDto;
import ee.hitsa.ois.web.dto.timetable.GroupTimetableDto;
import ee.hitsa.ois.web.dto.timetable.HigherTimetablePlanDto;
import ee.hitsa.ois.web.dto.timetable.RoomTimetableDto;
import ee.hitsa.ois.web.dto.timetable.TeacherTimetableDto;
import ee.hitsa.ois.web.dto.timetable.TimetableDatesDto;
import ee.hitsa.ois.web.dto.timetable.TimetableDto;
import ee.hitsa.ois.web.dto.timetable.TimetableImportDto;
import ee.hitsa.ois.web.dto.timetable.TimetableManagementSearchDto;
import ee.hitsa.ois.web.dto.timetable.TimetablePlanDto;
import ee.hitsa.ois.web.dto.timetable.TimetableStudyYearWeekDto;
import ee.hitsa.ois.web.dto.timetable.UntisCodeError;
import ee.hitsa.ois.web.dto.timetable.VocationalTimetablePlanDto;
import ee.hitsa.ois.xml.exportTimetable.Document;
import ee.hitsa.ois.service.timetable.TimetableExporter;

@RestController
@RequestMapping("/timetables")
public class TimetableController {
	
	@Autowired
    private XmlService xmlService;
    @Autowired
    private TimetableService timetableService;
    @Autowired
    private TimetableGenerationService timetableGenerationService;
    @Autowired
    private TimetableExporter timetableExporter;
    @Autowired
    private EntityManager em;

    @GetMapping("/{id:\\d+}")
    public TimetableDto edit(HoisUserDetails user, @WithEntity Timetable timetable) {
        UserUtil.assertIsSchoolAdmin(user, timetable.getSchool());
        return timetableService.get(user, timetable);
    }

    @GetMapping("/{id:\\d+}/view")
    public TimetableDto get(HoisUserDetails user, @WithEntity Timetable timetable) {
        UserUtil.assertSameSchool(user, timetable.getSchool());
        return timetableService.getForView(timetable);
    }
 
    @GetMapping("/{id:\\d+}/createVocationalPlan")
    public VocationalTimetablePlanDto createVocationalPlan(HoisUserDetails user, @WithEntity Timetable timetable) {
        UserUtil.assertIsSchoolAdmin(user, timetable.getSchool());
        return timetableService.getVocationalPlan(timetable);
    }
    
    @GetMapping("/{id:\\d+}/createHigherPlan")
    public HigherTimetablePlanDto createHigherPlan(HoisUserDetails user, @WithEntity Timetable timetable) {
        UserUtil.assertIsSchoolAdmin(user, timetable.getSchool());
        return timetableService.getHigherPlan(timetable);
    }

    @GetMapping("/managementSearchFormData")
    public Map<String, ?> managementSearchFormData(HoisUserDetails user) {
        UserUtil.assertIsSchoolAdmin(user);
        return timetableService.managementSearchFormData(user.getSchoolId());
    }

    @GetMapping("/searchTimetableForManagement")
    public Page<TimetableManagementSearchDto> searchTimetableForManagement(HoisUserDetails user,
            @Valid TimetableManagementSearchCommand criteria, Pageable pageable) {
        UserUtil.assertIsSchoolAdmin(user);
        return timetableService.searchTimetableForManagement(criteria, pageable, user);
    }

    @GetMapping("/blockedDatesForPeriod")
    public List<TimetableDatesDto> blockedDatesForPeriod(HoisUserDetails user,
            @RequestParam("studyPeriod") Long studyPeriod, @RequestParam("code") String code,
            @RequestParam(name = "currentTimetable", required = false) Long currentTimetable) {
        UserUtil.assertIsSchoolAdmin(user);
        return timetableService.blockedDatesForPeriod(user, studyPeriod, code, currentTimetable);
    }

    @PostMapping
    public TimetableDto create(HoisUserDetails user, @Valid @RequestBody TimetableEditForm form) {
        UserUtil.assertIsSchoolAdmin(user);
        return get(user, timetableService.createTimetable(user, form));
    }

    @PostMapping("/saveVocationalEvent")
    public TimetablePlanDto saveVocationalEvent(HoisUserDetails user,
            @Valid @RequestBody TimetableEventVocationalForm form) {
        UserUtil.assertIsSchoolAdmin(user);
        Timetable timetable = timetableService.saveVocationalEvent(form);
        // TODO: create a different smaller query for getting updated subjects after
        // saving a subject
        return createVocationalPlan(user, timetable);
    }

    @PostMapping("/saveHigherEvent")
    public TimetablePlanDto saveHigherEvent(HoisUserDetails user, @Valid @RequestBody TimetableEventHigherForm form) {
        UserUtil.assertIsSchoolAdmin(user);
        Timetable timetable = timetableService.saveHigherEvent(form);
        // TODO: create a different smaller query for getting updated subjects after
        // saving a subject
        return createHigherPlan(user, timetable);
    }

    @PostMapping("/deleteVocationalEvent")
    public TimetablePlanDto deleteVocationalEvent(HoisUserDetails user, @Valid @RequestBody TimetableRoomAndTimeForm form) {
        UserUtil.assertIsSchoolAdmin(user);
        return createVocationalPlan(user, timetableService.deleteEvent(user, form));
    }

    @PostMapping("/deleteHigherEvent")
    public TimetablePlanDto deleteHigherEvent(HoisUserDetails user, @Valid @RequestBody TimetableRoomAndTimeForm form) {
        UserUtil.assertIsSchoolAdmin(user);
        return createHigherPlan(user, timetableService.deleteEvent(user, form));
    }

    @GetMapping("/getPossibleTargetsForCopy")
    public List<TimetableManagementSearchDto> getPossibleTargetsForCopy(HoisUserDetails user, @RequestParam("id") Long timetableId) {
        UserUtil.assertIsSchoolAdmin(user);
        return timetableService.getPossibleTargetsForCopy(user, timetableId);
    }

    @GetMapping("/copyTimetable")
    public TimetableDto cloneTimetable(HoisUserDetails user, @Valid TimetableCopyForm form) {
        UserUtil.assertIsSchoolAdmin(user);
        return get(user, timetableService.cloneTimetable(user, form));
    }

    @PostMapping("/saveVocationalEventRoomsAndTimes")
    public TimetablePlanDto saveVocationalEventRoomsAndTimes(HoisUserDetails user,
            @Valid @RequestBody TimetableRoomAndTimeForm form) {
        UserUtil.assertIsSchoolAdmin(user);
        return createVocationalPlan(user, timetableService.saveEventRoomsAndTimes(user, form));
    }

    @PostMapping("/saveHigherEventRoomsAndTimes")
    public TimetablePlanDto saveHigherEventRoomsAndTimes(HoisUserDetails user,
            @Valid @RequestBody TimetableRoomAndTimeForm form) {
        UserUtil.assertIsSchoolAdmin(user);
        return createHigherPlan(user, timetableService.saveEventRoomsAndTimes(user, form));
    }

    @PutMapping("/{id:\\d+}")
    public TimetableDto save(HoisUserDetails user, @Valid @RequestBody TimetableEditForm form,
            @WithEntity Timetable timetable) {
        UserUtil.assertIsSchoolAdmin(user, timetable.getSchool());
        return get(user, timetableService.save(user, form, timetable));
    }

    @PutMapping("/{id:\\d+}/confirm")
    public TimetableDto confirm(HoisUserDetails user, @WithEntity Timetable timetable) {
        UserUtil.assertIsSchoolAdmin(user, timetable.getSchool());
        return get(user, timetableService.confirm(timetable));
    }

    @PutMapping("/{id:\\d+}/publicize")
    public TimetableDto publicize(HoisUserDetails user, @WithEntity Timetable timetable) {
        UserUtil.assertIsSchoolAdmin(user, timetable.getSchool());
        return get(user, timetableService.publicize(timetable));
    }

    @GetMapping("/timetableStudyYears/{school:\\d+}")
    public List<StudyYearSearchDto> timetableStudyYears(@WithEntity("school") School school) {
        return timetableService.timetableStudyYears(school);
    }

    @GetMapping("/timetableStudyYears/person")
    public List<StudyYearSearchDto> personTimetableStudyYears(@RequestParam("encodedPerson") String encodedPerson) {
        return timetableService.personTimetableStudyYears(encodedPerson);
    }

    @GetMapping("/timetableStudyYearWeeks/{studyYear:\\d+}")
    public List<TimetableStudyYearWeekDto> timetableStudyYearWeeks(@WithEntity("studyYear") StudyYear studyYear,
            @RequestParam(required = false) @WithEntity("student") Student student) {
        return timetableService.timetableStudyYearWeeks(studyYear, student);
    }

    @GetMapping("/timetableStudyYearWeeks/{studyYear:\\d+}/person")
    public List<TimetableStudyYearWeekDto> personTimetableStudyYearWeeks(@WithEntity("studyYear") StudyYear studyYear,
            @RequestParam("encodedPerson") String encodedPerson) {
        return timetableService.personTimetableStudyYearWeeks(studyYear, encodedPerson);
    }

    @GetMapping("/group/{school:\\d+}/{studyYear:\\d+}")
    public List<GroupTimetableDto> groupTimetables(@WithEntity("school") School school,
            @PathVariable("studyYear") Long studyYearId) {
        return timetableService.groupTimetables(school, studyYearId);
    }

    @GetMapping("/teacher/{school:\\d+}/{studyYear:\\d+}")
    public List<TeacherTimetableDto> teacherTimetables(@WithEntity("school") School school,
            @PathVariable("studyYear") Long studyYearId) {
        return timetableService.teacherTimetables(school, studyYearId);
    }

    @GetMapping("/room/{school:\\d+}/{studyYear:\\d+}")
    public List<RoomTimetableDto> roomTimetables(@WithEntity("school") School school,
            @PathVariable("studyYear") Long studyYearId) {
        return timetableService.roomTimetables(school, studyYearId);
    }

    @GetMapping("/timetableDifference.xls")
    public void timetableDifferenceExcel(HoisUserDetails user, @RequestParam("studyPeriod") @WithEntity("studyPeriod") StudyPeriod studyPeriod,
            @RequestParam("startDate") LocalDate startDate, HttpServletResponse response) throws IOException {
        UserUtil.assertIsSchoolAdmin(user, studyPeriod.getStudyYear().getSchool());
        HttpUtil.xls(response, "timetableDifference.xls", timetableGenerationService.timetableDifferenceExcel(studyPeriod, startDate));
    }

    @GetMapping("/timetablePlan.xlsx")
    public void timetablePlan(HoisUserDetails user, @RequestParam("id") Long id, HttpServletResponse response)
            throws IOException {
        UserUtil.assertIsSchoolAdmin(user);
        HttpUtil.xls(response, "timetablePlan.xlsx", timetableGenerationService.timetablePlanExcel(id));
    }

    /**
     * Untis:
     * Get exported week from timetable with teachers, classes, rooms, lessons
     * Parameters can be set in Untis after importing this file
     * 
     * aSc Timetable:
     * Get exported week from timetable with teachers, classes, subjects and amount of lessons
     * 
     * XXX: Find the problem in frontend with corrupted excel file if link does not have extension
     * 
     * @param user
     * @param startDate
     * @param endDate
     * @param studyPeriod
     * @param response
     * @throws IOException
     */
    @GetMapping("/exportTimetable.xlsx")
    public void exportTimetable(HoisUserDetails user, @RequestParam("startDate") LocalDate startDate,
            @RequestParam("endDate") LocalDate endDate,
            @RequestParam("studyPeriod") @WithEntity("studyPeriod") StudyPeriod studyPeriod,
            HttpServletResponse response) throws IOException {
        UserUtil.assertIsSchoolAdmin(user);
        School school = em.getReference(School.class, user.getSchoolId());
        TimetableUserRights.assertCanImportOrExportTimetable(user, school);
        try {
            if (ClassifierUtil.equals(SchoolTimetableType.TIMETABLE_UNTIS, school.getTimetable())) {
                Document document = timetableService.getExportedWeek(startDate, endDate, studyPeriod, user);
                HttpUtil.xml(response, startDate.toString() + "-" + endDate.toString() + ".xml",
                        xmlService.generateFromObject(document));
            } else if (ClassifierUtil.equals(SchoolTimetableType.TIMETABLE_ASC, school.getTimetable())) {
                HttpUtil.xlsx(response, startDate.toString() + "-" + endDate.toString() + ".xlsx",
                        timetableGenerationService.timetableAscExport(startDate, studyPeriod, user));
            } else {
                throw new HoisException("This school timetable type is not supported or/and not used in the system.");
            }
        } catch (JAXBException e) {
            throw new HoisException(e);
        }
    }
    
    /**
     * Check if student group teachers, journals and journal teachers have untis codes.
     * 
     * @param user
     * @param startDate
     * @param studyPeriod
     * @return Possible errors
     */
    @GetMapping("/exportTimetableCheck")
    public UntisCodeError exportTimetableCheck(HoisUserDetails user, @RequestParam("startDate") LocalDate startDate, @RequestParam("studyPeriod") @WithEntity("studyPeriod") StudyPeriod studyPeriod) {
    	UserUtil.assertIsSchoolAdmin(user);
    	return timetableService.checkExportPossibilites(startDate, studyPeriod, user);
    }
    
    /**
     * Import XML file to a timetable week (mark hours, teachers, capacity).
     * 
     * @param user
     * @param dto
     * @return Possible errors in decoding and parsing XML
     */
    @PostMapping("/importXml")
    public TimetableImportErrorDto importXml(HoisUserDetails user, @Valid @RequestBody TimetableImportDto dto) {
        School school = em.getReference(School.class, user.getSchoolId());
        TimetableUserRights.assertCanImportOrExportTimetable(user, school);
    	TimetableImportErrorDto errorDto = new TimetableImportErrorDto();
    	errorDto.setType(EnumUtil.valueOf(SchoolTimetableType.class, school.getTimetable())); 
    	errorDto.setMessages(timetableExporter.importXml(user, dto));
    	return errorDto;
    }

}
