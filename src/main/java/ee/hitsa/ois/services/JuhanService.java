package ee.hitsa.ois.services;

import static ee.hitsa.ois.util.JpaQueryUtil.resultAsLocalDateTime;
import static ee.hitsa.ois.util.JpaQueryUtil.resultAsLong;
import static ee.hitsa.ois.util.JpaQueryUtil.resultAsString;

import ee.hitsa.ois.domain.BaseEntityWithId;
import ee.hitsa.ois.domain.Classifier;
import ee.hitsa.ois.domain.Room;
import ee.hitsa.ois.domain.WsJuhanLog;
import ee.hitsa.ois.domain.school.School;
import ee.hitsa.ois.domain.teacher.Teacher;
import ee.hitsa.ois.domain.timetable.TimetableEvent;
import ee.hitsa.ois.domain.timetable.TimetableEventRoom;
import ee.hitsa.ois.domain.timetable.TimetableEventTeacher;
import ee.hitsa.ois.domain.timetable.TimetableEventTime;
import ee.hitsa.ois.enums.TimetableEventRepeat;
import ee.hitsa.ois.util.DateUtils;
import ee.hitsa.ois.util.EntityUtil;
import ee.hitsa.ois.util.JpaNativeQueryBuilder;
import ee.hitsa.ois.util.JpaQueryBuilder;
import ee.hitsa.ois.util.StreamUtil;
import ee.hitsa.ois.validation.Required;
import ee.hitsa.ois.validation.ValidationFailedException;
import ee.hitsa.ois.web.commandobject.juhan.JuhanEventForm;
import ee.hitsa.ois.web.commandobject.juhan.JuhanEventRoomForm;
import ee.hitsa.ois.web.commandobject.juhan.JuhanEventTeacherForm;
import ee.hitsa.ois.web.commandobject.juhan.JuhanEventsForm;
import ee.hitsa.ois.web.commandobject.juhan.JuhanRoomCommand;
import ee.hitsa.ois.web.commandobject.juhan.JuhanTeacherCommand;
import ee.hitsa.ois.web.dto.BusyTimeDto;
import ee.hitsa.ois.web.dto.juhan.JuhanBuildingDto;
import ee.hitsa.ois.web.dto.juhan.JuhanRoomDto;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.GrantedAuthority;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Service;

import javax.persistence.EntityManager;
import javax.transaction.Transactional;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

@Transactional
@Service
public class JuhanService {

    private static final String FAIL = "NOT_FOUND";
    private static final String OK = "OK";
    private static final Authentication JUHAN = new UsernamePasswordAuthenticationToken("JUHAN", null,
            Collections.singletonList((GrantedAuthority)(() -> "ROLE_JUHAN")));

    @Autowired
    private EntityManager em;

    public ResponseEntity<Map<String, Object>> teacher(Long schoolId, JuhanTeacherCommand criteria) {
        if (criteria.getIdcode() != null && criteria.getUqcode() != null) {
            throw new ValidationFailedException("Teacher can't be found by both idcode and uqcode");
        }
        JpaNativeQueryBuilder qb = new JpaNativeQueryBuilder("from teacher t join person p on p.id = t.person_id");
        qb.requiredCriteria("t.school_id = :schoolId", "schoolId", schoolId);
        qb.optionalCriteria("p.idcode = :idcode", "idcode", criteria.getIdcode());
        qb.optionalCriteria("p.unique_code = :uniqueCode", "uniqueCode", criteria.getUqcode());
        List<?> data = qb.select("t.id", em).setMaxResults(1).getResultList();

        List<BusyTimeDto> busyTimes = new ArrayList<>();
        if (!data.isEmpty()) {
            Long teacherId = resultAsLong(data.get(0), 0);
            busyTimes = teacherBusyTimes(schoolId, criteria, teacherId);
        }

        boolean resultOk = !data.isEmpty();
        Map<String, Object> result = new HashMap<>();
        result.put("result", resultOk ? OK : FAIL);
        if (resultOk) {
            boolean isBusy = !busyTimes.isEmpty();
            result.put("isBusy", Boolean.valueOf(isBusy));
            if (isBusy) {
                result.put("busyTimes", busyTimes);
            }
        }
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    private List<BusyTimeDto> teacherBusyTimes(Long schoolId, JuhanTeacherCommand criteria, Long teacherId) {
        JpaNativeQueryBuilder qb = new JpaNativeQueryBuilder("from timetable_event te " +
                "join timetable_event_time tem on te.id = tem.timetable_event_id " +
                "join timetable_event_teacher tet on tem.id = tet.timetable_event_time_id " +
                "join teacher t on t.id = tet.teacher_id");
        qb.requiredCriteria("t.school_id = :schoolId", "schoolId", schoolId);
        qb.requiredCriteria("t.id = :teacherId", "teacherId", teacherId);
        qb.requiredCriteria("tem.start < :eventEnd", "eventEnd", DateUtils.toLocalDateTime(criteria.getEventEnd()));
        qb.requiredCriteria("tem.end > :eventStart", "eventStart", DateUtils.toLocalDateTime(criteria.getEventStart()));

        qb.sort("tem.start, tem.end");
        List<?> data = qb.select("tem.id, tem.start, tem.end", em).getResultList();
        return StreamUtil.toMappedList(r -> new BusyTimeDto(DateUtils.toISOString(resultAsLocalDateTime(r, 1)),
                        DateUtils.toISOString(resultAsLocalDateTime(r, 2))), data);
    }

    public Map<String, Object> buildings(Long schoolId) {
        JpaNativeQueryBuilder qb = new JpaNativeQueryBuilder("from building b");
        qb.requiredCriteria("b.school_id = :schoolId", "schoolId", schoolId);

        qb.sort("b.code, b.name");
        List<?> data = qb.select("b.id, b.code, b.name, b.address_ads_oid, b.address", em).getResultList();
        List<JuhanBuildingDto> buildings = StreamUtil.toMappedList(r -> new JuhanBuildingDto(resultAsLong(r, 0),
                resultAsString(r, 1), resultAsString(r, 2), resultAsString(r, 3), resultAsString(r, 4)), data);

        boolean resultOk = !buildings.isEmpty();
        Map<String, Object> result = new HashMap<>();
        result.put("result", resultOk ? OK : FAIL);
        if (resultOk) {
            result.put("buildings", buildings);
        }
        return result;
    }

    public Map<String, Object> rooms(Long schoolId, JuhanRoomCommand criteria) {
        JpaNativeQueryBuilder qb = new JpaNativeQueryBuilder("from room r join building b on b.id = r.building_id");
        qb.requiredCriteria("b.school_id = :schoolId", "schoolId", schoolId);
        qb.requiredCriteria("b.id = :buildingId", "buildingId", criteria.getBuildingId());

        qb.sort("b.code, r.code, r.name");
        List<?> data = qb.select("r.id r_id, r.code, r.name", em).getResultList();
        Map<Long, JuhanRoomDto> rooms = StreamUtil.toMap(r -> resultAsLong(r, 0), r -> {
            JuhanRoomDto room = new JuhanRoomDto();
            room.setId(resultAsLong(r, 0));
            String code = resultAsString(r, 1);
            room.setCode(code);
            String name = resultAsString(r, 2);
            room.setName(name != null ? name : code);
            return room;
        }, data);

        if (!rooms.isEmpty()) {
            setRoomBusyTimes(criteria, rooms);
        }

        boolean resultOk = !rooms.isEmpty();
        Map<String, Object> result = new HashMap<>();
        result.put("result", resultOk ? OK : FAIL);
        if (resultOk) {
            result.put("rooms", rooms.values());
        }
        return result;
    }

    private void setRoomBusyTimes(JuhanRoomCommand criteria, Map<Long, JuhanRoomDto> rooms) {
        JpaNativeQueryBuilder qb = new JpaNativeQueryBuilder("from timetable_event te "
                + "join timetable_event_time tem on te.id = tem.timetable_event_id "
                + "join timetable_event_room ter on tem.id = ter.timetable_event_time_id");
        qb.requiredCriteria("ter.room_id in (:roomIds)", "roomIds", rooms.keySet());
        qb.requiredCriteria("tem.start < :eventEnd", "eventEnd", DateUtils.toLocalDateTime(criteria.getEventEnd()));
        qb.requiredCriteria("tem.end > :eventStart", "eventStart", DateUtils.toLocalDateTime(criteria.getEventStart()));

        qb.sort("tem.start, tem.end");
        List<?> data = qb.select("ter.room_id, tem.start, tem.end", em).getResultList();
        Map<Long, List<BusyTimeDto>> busyTimes = data.stream().collect(Collectors.groupingBy(r -> resultAsLong(r, 0),
                Collectors.mapping(r -> new BusyTimeDto(DateUtils.toISOString(resultAsLocalDateTime(r, 1)),
                        DateUtils.toISOString(resultAsLocalDateTime(r, 2))), Collectors.toList())));

        for (Long roomId : rooms.keySet()) {
            JuhanRoomDto room = rooms.get(roomId);
            List<BusyTimeDto> roomBusyTimes = busyTimes.get(roomId);
            if (roomBusyTimes != null) {
                room.setIsBusy(Boolean.TRUE);
                room.setBusyTimes(roomBusyTimes);
            } else {
                room.setIsBusy(Boolean.FALSE);
            }
        }
    }

    public ResponseEntity<Map<String, Object>> events(JuhanEventsForm eventsForm) {
        // hack: we are going to change authentication to allow audit info filled
        SecurityContextHolder.getContext().setAuthentication(JUHAN);
        Authentication oldAuthentication = SecurityContextHolder.getContext().getAuthentication();

        Set<Long> juhanEventIds = StreamUtil.toMappedSet(JuhanEventForm::getEventId, eventsForm.getEvents());
        if (juhanEventIds.size() != eventsForm.getEvents().size()) {
            throw new ValidationFailedException("Duplicate event ids");
        }

        Map<String, Object> result = new HashMap<>();
        List<Map<String, Object>> events = new ArrayList<>();
        Map<Long, TimetableEvent> createdEvents = juhanEvents(juhanEventIds);

        for (JuhanEventForm eventForm : eventsForm.getEvents()) {
            TimetableEvent timetableEvent = createdEvents.get(eventForm.getEventId());
            if (timetableEvent != null && !eventForm.getSchoolId().equals(EntityUtil.getId(timetableEvent.getSchool()))) {
                throw new ValidationFailedException("Event and school don't match");
            }

            if (timetableEvent != null && eventForm.getTeachers().isEmpty() && eventForm.getRooms().isEmpty()) {
                EntityUtil.deleteEntity(timetableEvent, em);
                events.add(deleteEvent(timetableEvent));
            } else {
                List<String> requiredFields = eventFormRequiredFieldsValidation(eventForm);
                if (!requiredFields.isEmpty()) {
                    return new ResponseEntity<>(errorResponse(requiredFields), HttpStatus.PRECONDITION_FAILED);
                }
                events.add(createEvent(timetableEvent, eventForm));
            }
        }
        result.put("events", events);

        SecurityContextHolder.getContext().setAuthentication(oldAuthentication);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    private List<String> eventFormRequiredFieldsValidation(JuhanEventForm eventForm) {
        List<String> requiredFields = new ArrayList<>();
        if (eventForm.getEventName() == null) {
            requiredFields.add("eventName");
        }
        if (eventForm.getEventStart() == null) {
            requiredFields.add("eventStart");
        }
        if (eventForm.getEventEnd() == null) {
            requiredFields.add("eventEnd");
        }
        return requiredFields;
    }

    private Map<String, Object> deleteEvent(TimetableEvent timetableEvent) {
        Map<String, Object> result = new HashMap<>();
        result.put("eventId", timetableEvent.getJuhanEventId());
        try {
            EntityUtil.deleteEntity(timetableEvent, em);
            result.put("result", OK);
        } catch (Exception e) {
            result.put("result", FAIL);
            result.put("msg", e.getMessage());
        }
        return result;
    }

    private Map<String, Object> createEvent(TimetableEvent timetableEvent, JuhanEventForm eventForm) {
        Map<String, Object> result = new HashMap<>();
        result.put("eventId", eventForm.getEventId());
        try {
            if (timetableEvent == null) {
                timetableEvent = new TimetableEvent();
            }

            LocalDateTime start = DateUtils.toLocalDateTime(eventForm.getEventStart());
            LocalDateTime end = DateUtils.toLocalDateTime(eventForm.getEventEnd());

            List<Teacher> formTeachers = formTeachers(eventForm);
            List<Room> formRooms = formRooms(eventForm);
            if (formTeachers.isEmpty() && formRooms.isEmpty()) {
                throw new ValidationFailedException("Missing a school connected teacher or room");
            }

            timetableEvent.setSchool(em.getReference(School.class, eventForm.getSchoolId()));
            timetableEvent.setRepeatCode(em.getReference(Classifier.class, TimetableEventRepeat.TUNNIPLAAN_SYNDMUS_KORDUS_EI.name()));
            timetableEvent.setStart(start);
            timetableEvent.setEnd(end);
            timetableEvent.setJuhanEventId(eventForm.getEventId());

            TimetableEventTime timetableEventTime = !timetableEvent.getTimetableEventTimes().isEmpty() ?
                    timetableEvent.getTimetableEventTimes().get(0) : new TimetableEventTime();
            timetableEventTime.setTimetableEvent(timetableEvent);
            timetableEvent.setName(maximumSubstring(eventForm.getEventName(), 255));
            timetableEventTime.setStart(start);
            timetableEventTime.setEnd(end);

            EntityUtil.bindEntityCollection(timetableEventTime.getTimetableEventTeachers(),
                    r -> EntityUtil.getId(r.getTeacher()), formTeachers, BaseEntityWithId::getId, teacher -> {
                        TimetableEventTeacher eventTeacher = new TimetableEventTeacher();
                        eventTeacher.setTeacher(teacher);
                        eventTeacher.setTimetableEventTime(timetableEventTime);
                        return eventTeacher;
                    });
            String otherTeachers = otherTeachers(eventForm, formTeachers);
            if (!otherTeachers.isEmpty()) {
                timetableEventTime.setOtherTeacher(maximumSubstring(otherTeachers, 1000));
            }

            EntityUtil.bindEntityCollection(timetableEventTime.getTimetableEventRooms(),
                    r -> EntityUtil.getId(r.getRoom()), formRooms, BaseEntityWithId::getId, room -> {
                        TimetableEventRoom eventRoom = new TimetableEventRoom();
                        eventRoom.setRoom(room);
                        eventRoom.setTimetableEventTime(timetableEventTime);
                        return eventRoom;
                    });
            String otherRooms = StreamUtil.nullSafeList(eventForm.getRooms()).stream().filter(
                    r -> r.getRoomId() == null && r.getRoomName() != null)
                    .map(JuhanEventRoomForm::getRoomName).collect(Collectors.joining(", "));
            if (!otherRooms.isEmpty()) {
                timetableEventTime.setOtherRoom(maximumSubstring(otherRooms, 1000));
            }

            timetableEvent.getTimetableEventTimes().add(timetableEventTime);
            EntityUtil.save(timetableEvent, em);
            result.put("result", OK);
        } catch (Exception e) {
            result.put("result", FAIL);
            result.put("msg", e.getMessage());
        }
        return result;
    }

    private Map<Long, TimetableEvent> juhanEvents(Set<Long> juhanEventIds) {
        if (juhanEventIds.isEmpty()) {
            return new HashMap<>();
        }
        List<TimetableEvent> events = em.createQuery("select te from TimetableEvent te " +
                "where te.juhanEventId in ?1", TimetableEvent.class)
                .setParameter(1, juhanEventIds)
                .getResultList();
        return StreamUtil.toMap(TimetableEvent::getJuhanEventId, e -> e, events);
    }

    private List<Teacher> formTeachers(JuhanEventForm eventForm) {
        List<Teacher> teachers = new ArrayList<>();

        List<String> idcodes = StreamUtil.nullSafeList(eventForm.getTeachers()).stream()
                .filter(t -> t.getIdcode() != null).map(JuhanEventTeacherForm::getIdcode)
                .collect(Collectors.toList());
        List<String> uniqueCodes = StreamUtil.nullSafeList(eventForm.getTeachers()).stream()
                .filter(t -> t.getUqcode() != null).map(JuhanEventTeacherForm::getUqcode)
                .collect(Collectors.toList());
        if (!idcodes.isEmpty() || !uniqueCodes.isEmpty()) {
            JpaQueryBuilder<Teacher> qb = new JpaQueryBuilder<>(Teacher.class, "t");
            qb.requiredCriteria("t.school.id = :schoolId", "schoolId", eventForm.getSchoolId());
            if (uniqueCodes.isEmpty()) {
                qb.optionalCriteria("t.person.idcode in (:idcodes)", "idcodes", idcodes);
            } else if (idcodes.isEmpty()) {
                qb.optionalCriteria("t.person.uniqueCode in (:uniqueCodes)", "uniqueCodes", uniqueCodes);
            } else {
                qb.filter("(t.person.idcode in (:idcodes) or t.person.uniqueCode in (:uniqueCodes))");
                qb.parameter("idcodes", idcodes);
                qb.parameter("uniqueCodes", uniqueCodes);
            }

            teachers = qb.select(em).getResultList();
        }
        return teachers;
    }

    private String otherTeachers(JuhanEventForm eventForm, List<Teacher> formTeachers) {
        List<String> teacherCodes = formTeacherCodes(formTeachers);
        return eventForm.getTeachers().stream().filter(t -> t.getTeacherName() != null
                && ((t.getIdcode() == null && t.getUqcode() == null)
                || (!teacherCodes.contains(t.getIdcode()) && !teacherCodes.contains(t.getUqcode()))))
                .map(JuhanEventTeacherForm::getTeacherName).collect(Collectors.joining(", "));
    }

    private List<String> formTeacherCodes(List<Teacher> formTeachers) {
        if (formTeachers.isEmpty()) {
            return new ArrayList<>();
        }
        List<?> data = em.createNativeQuery("select coalesce(p.unique_code, p.idcode) from teacher t "
                + "join person p on p.id = t.person_id where t.id in ?1")
                .setParameter(1, StreamUtil.toMappedList(BaseEntityWithId::getId, formTeachers))
                .getResultList();
        return StreamUtil.toMappedList(r -> resultAsString(r, 0), data);
    }

    private List<Room> formRooms(JuhanEventForm eventForm) {
        List<Room> rooms = new ArrayList<>();

        List<Long> roomIds = StreamUtil.nullSafeList(eventForm.getRooms()).stream()
                .filter(r -> r.getRoomId() != null).map(JuhanEventRoomForm::getRoomId)
                .collect(Collectors.toList());
        if (!roomIds.isEmpty()) {
            JpaQueryBuilder<Room> qb = new JpaQueryBuilder<>(Room.class, "r");
            qb.requiredCriteria("r.building.school.id = :schoolId", "schoolId", eventForm.getSchoolId());
            qb.optionalCriteria("r.id in (:roomIds)", "roomIds", roomIds);

            rooms = qb.select(em).getResultList();
        }
        if (rooms.size() != roomIds.size()) {
            throw new ValidationFailedException("Event rooms don't belong to school");
        }
        return rooms;
    }

    private Map<String, Object> errorResponse(List<String> requiredFields) {
        List<Map<String, String>> errors = new ArrayList<>();
        for (String field : requiredFields) {
            Map<String, String> error = new HashMap<>();
            error.put("code", Required.MESSAGE);
            error.put("field", field);
            errors.add(error);
        }
        return Collections.singletonMap("_errors", errors);
    }

    private String maximumSubstring(String field, int maxLength) {
        return field.length() > maxLength ? field.substring(0, maxLength) : field;
    }

    public void saveJuhanLog(WsJuhanLog log) {
        em.persist(log);
    }

}