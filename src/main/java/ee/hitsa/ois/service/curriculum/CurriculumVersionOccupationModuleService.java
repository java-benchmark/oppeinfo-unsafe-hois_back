package ee.hitsa.ois.service.curriculum;

import static ee.hitsa.ois.util.JpaQueryUtil.resultAsLong;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;

import javax.persistence.EntityManager;
import javax.persistence.PersistenceException;
import javax.transaction.Transactional;

import org.hibernate.exception.ConstraintViolationException;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import ee.hitsa.ois.domain.Classifier;
import ee.hitsa.ois.domain.basemodule.BaseModule;
import ee.hitsa.ois.domain.curriculum.CurriculumModule;
import ee.hitsa.ois.domain.curriculum.CurriculumModuleOutcome;
import ee.hitsa.ois.domain.curriculum.CurriculumVersion;
import ee.hitsa.ois.domain.curriculum.CurriculumVersionOccupationModule;
import ee.hitsa.ois.domain.curriculum.CurriculumVersionOccupationModuleCapacity;
import ee.hitsa.ois.domain.curriculum.CurriculumVersionOccupationModuleOutcome;
import ee.hitsa.ois.domain.curriculum.CurriculumVersionOccupationModuleTheme;
import ee.hitsa.ois.domain.curriculum.CurriculumVersionOccupationModuleThemeCapacity;
import ee.hitsa.ois.domain.curriculum.CurriculumVersionOccupationModuleYearCapacity;
import ee.hitsa.ois.domain.teacher.Teacher;
import ee.hitsa.ois.enums.VocationalGradeType;
import ee.hitsa.ois.exception.EntityRemoveException;
import ee.hitsa.ois.repository.ClassifierRepository;
import ee.hitsa.ois.service.AutocompleteService;
import ee.hitsa.ois.service.security.HoisUserDetails;
import ee.hitsa.ois.util.BaseModuleUtil;
import ee.hitsa.ois.util.ClassifierUtil;
import ee.hitsa.ois.util.CurriculumUtil;
import ee.hitsa.ois.util.EntityUtil;
import ee.hitsa.ois.util.OccupationModuleCapacitiesUtil;
import ee.hitsa.ois.util.StreamUtil;
import ee.hitsa.ois.validation.ValidationFailedException;
import ee.hitsa.ois.web.commandobject.SchoolCapacityTypeCommand;
import ee.hitsa.ois.web.dto.curriculum.CurriculumModuleOutcomeDto;
import ee.hitsa.ois.web.dto.curriculum.CurriculumVersionOccupationModuleCapacityDto;
import ee.hitsa.ois.web.dto.curriculum.CurriculumVersionOccupationModuleDto;
import ee.hitsa.ois.web.dto.curriculum.CurriculumVersionOccupationModuleThemeCapacityDto;
import ee.hitsa.ois.web.dto.curriculum.CurriculumVersionOccupationModuleThemeDto;
import ee.hitsa.ois.web.dto.curriculum.CurriculumVersionOccupationModuleYearCapacityDto;

@Transactional
@Service
public class CurriculumVersionOccupationModuleService {

    @Autowired
    private EntityManager em;
    @Autowired
    private ClassifierRepository classifierRepository;
    @Autowired
    private AutocompleteService autocompleteService;
    
    private static final int MIN_MODULES_FOR_DELETION = 2;

    private List<Classifier> getCapacityTypes(Long schoolId) {
        SchoolCapacityTypeCommand command = new SchoolCapacityTypeCommand();
        command.setIsHigher(Boolean.FALSE);
        return autocompleteService.schoolCapacityTypes(schoolId, command);
    }

    /**
     * Sets empty capacities to occupation modules and themes
     */
    public CurriculumVersionOccupationModuleDto get(CurriculumVersionOccupationModule module) {
        CurriculumVersionOccupationModuleDto dto = CurriculumVersionOccupationModuleDto.of(module);

        List<Classifier> capacityTypes = getCapacityTypes(EntityUtil.getId(module.getCurriculumVersion().getCurriculum().getSchool()));

        OccupationModuleCapacitiesUtil.setEmptyModuleCapacities(dto, capacityTypes);
        for(CurriculumVersionOccupationModuleThemeDto themeDto : dto.getThemes()) {
            OccupationModuleCapacitiesUtil.setEmptyThemeCapacities(themeDto, capacityTypes);
        }
        return dto;
    }

    public CurriculumVersionOccupationModule create(CurriculumVersionOccupationModuleDto dto) {
        CurriculumVersion curriculumVersion = em.getReference(CurriculumVersion.class, dto.getCurriculumVersion());
        CurriculumVersionOccupationModule occupationModule = createOccupationModule(curriculumVersion, dto);
        curriculumVersion.getOccupationModules().add(occupationModule);
        return EntityUtil.save(occupationModule, em);
    }

    public CurriculumVersionOccupationModule update(CurriculumVersionOccupationModuleDto dto, CurriculumVersionOccupationModule occupationModule) {
        return EntityUtil.save(updateOccupationModule(dto, occupationModule), em);
    }
    
    public void delete(HoisUserDetails user, CurriculumVersionOccupationModule occupationModule) {
        CurriculumVersion curriculumVersion = occupationModule.getCurriculumVersion();
        if (curriculumVersion.getOccupationModules().size() < MIN_MODULES_FOR_DELETION) {
            throw new ValidationFailedException("curriculum.error.implementationPlanAtleastOneModuleNeeded");
        }
        
        EntityUtil.setUsername(user.getUsername(), em);
        try {
            curriculumVersion.getOccupationModules().remove(occupationModule);
            em.flush();
        } catch(PersistenceException e) {
            Throwable cause = e.getCause();
            if(cause instanceof ConstraintViolationException) {
                throw new EntityRemoveException(null, cause);
            }
            throw e;
        }
    }

    private CurriculumVersionOccupationModule createOccupationModule(CurriculumVersion curriculumVersion, CurriculumVersionOccupationModuleDto dto) {
        CurriculumVersionOccupationModule occupationModule = new CurriculumVersionOccupationModule();
        occupationModule.setCurriculumModule(em.getReference(CurriculumModule.class, dto.getCurriculumModule()));
        occupationModule.setCurriculumVersion(curriculumVersion);
        return updateOccupationModule(dto, occupationModule);
    }

    private CurriculumVersionOccupationModule updateOccupationModule(CurriculumVersionOccupationModuleDto dto, CurriculumVersionOccupationModule occupationModule) {
        if (dto.getCopy().equals(Boolean.FALSE) && dto.getBaseModule() != null 
                && !(occupationModule.getBaseModule() != null && occupationModule.getBaseModule().getId().equals(dto.getBaseModule()))) {
            BaseModule baseModule = em.getReference(BaseModule.class, dto.getBaseModule());
            occupationModule.setBaseModule(baseModule);
        }
        if (occupationModule.getBaseModule() == null && dto.getCopy().equals(Boolean.FALSE)) {
            EntityUtil.bindToEntity(dto, occupationModule,
                    classifierRepository, "curriculumModule", "capacities", "themes", "yearCapacities", "teacher");
            if (dto.getTeacher() != null && dto.getTeacher().getId() != null) {
                occupationModule.setTeacher(em.getReference(Teacher.class, dto.getTeacher().getId()));
                occupationModule.setSupervisor(occupationModule.getTeacher().getPerson().getFullname());
            }
        } else {
            EntityUtil.bindToEntity(dto, occupationModule,
                    classifierRepository, "curriculumModule", "capacities", "themes", "yearCapacities", "nameEt", "nameEn", "requirementsEt",
                    "assessmentsEt", "learningMethodsEt", "assessmentMethodsEt", "assessment", "totalGradeDescription", "passDescription",
                    "grade3Description", "grade4Description", "grade5Description", "independentStudyEt", "studyMaterials", "supervisor", "teacher");
            BaseModule baseModule = em.getReference(BaseModule.class, dto.getBaseModule());
            BaseModuleUtil.updateReferences(baseModule, Collections.emptySet(), Collections.singleton(occupationModule), em);
            if (occupationModule.getId() == null) {
                occupationModule.setThemes(StreamUtil.toMappedSet(t -> {
                    CurriculumVersionOccupationModuleTheme theme = BaseModuleUtil.themeTransform(t, occupationModule);
                    if (dto.getCopy().equals(Boolean.TRUE)) {
                        theme.setBaseModuleTheme(null);
                    }
                    return theme;
                }, baseModule.getThemes()));
                if (!occupationModule.getThemes().isEmpty()) {
                    OccupationModuleCapacitiesUtil.updateModuleCapacities(occupationModule, getCapacityTypes(baseModule.getSchool().getId()));
                }
            }
        }
        updateModuleCapacities(occupationModule, dto.getCapacities());
        updateYearCapacities(occupationModule, dto.getYearCapacities());
        return occupationModule;
    }

    private void updateYearCapacities(CurriculumVersionOccupationModule occupationModule,
            Set<CurriculumVersionOccupationModuleYearCapacityDto> yearCapacities) {
        Set<CurriculumVersionOccupationModuleYearCapacity> newOccupationModuleYearCapacities =
                yearCapacities.stream().filter(Objects::nonNull)
            .map(capacityDto -> updateYearCapacities(capacityDto, occupationModule)).collect(Collectors.toSet());
        occupationModule.setYearCapacities(newOccupationModuleYearCapacities);
    }

    private void updateModuleCapacities(CurriculumVersionOccupationModule occupationModule,
            Set<CurriculumVersionOccupationModuleCapacityDto> capacities) {
        
        Set<CurriculumVersionOccupationModuleCapacityDto> notEmptyCapacities = capacities.stream()
                .filter(c -> c.getHours() != null).collect(Collectors.toSet());

        Set<CurriculumVersionOccupationModuleCapacity> newOccupationModuleCapacities =
                notEmptyCapacities.stream().filter(Objects::nonNull)
            .map(capacityDto -> updateCapacities(capacityDto, occupationModule)).collect(Collectors.toSet());
        occupationModule.setCapacities(newOccupationModuleCapacities);
    }

    private CurriculumVersionOccupationModuleCapacity updateCapacities(CurriculumVersionOccupationModuleCapacityDto dto,
            CurriculumVersionOccupationModule updatedOccupationModule) {
        Optional<CurriculumVersionOccupationModuleCapacity> o = dto.getId() == null ? Optional.empty() :
            updatedOccupationModule.getCapacities().stream().filter(c -> c.getId().equals(dto.getId())).findFirst();

        return EntityUtil.bindToEntity(dto, o.isPresent() ? o.get() : new CurriculumVersionOccupationModuleCapacity(), classifierRepository);
    }

    private CurriculumVersionOccupationModuleYearCapacity updateYearCapacities(
            CurriculumVersionOccupationModuleYearCapacityDto dto,
            CurriculumVersionOccupationModule updatedOccupationModule) {
        CurriculumVersionOccupationModuleYearCapacity capacity = dto.getId() == null ? new CurriculumVersionOccupationModuleYearCapacity() :
            EntityUtil.find(dto.getId(), updatedOccupationModule.getYearCapacities()).get();
        return EntityUtil.bindToEntity(dto, capacity, classifierRepository);
    }

    private void updateThemeOutcomes(CurriculumVersionOccupationModuleTheme theme, Set<Long> outcomes) {
        EntityUtil.bindEntityCollection(theme.getOutcomes(), o -> EntityUtil.getId(o.getOutcome()), outcomes, d -> {
            CurriculumVersionOccupationModuleOutcome outcome = new CurriculumVersionOccupationModuleOutcome();
            outcome.setOutcome(em.getReference(CurriculumModuleOutcome.class, d));
            return outcome;
        });
    }

    public CurriculumVersionOccupationModuleThemeDto getTheme(CurriculumVersionOccupationModuleTheme theme) {
        List<Classifier> capacityTypes = getCapacityTypes(EntityUtil.getId(theme.getModule().getCurriculumVersion().getCurriculum().getSchool()));

        CurriculumVersionOccupationModuleThemeDto themeDto = CurriculumVersionOccupationModuleThemeDto.of(theme);
        OccupationModuleCapacitiesUtil.setEmptyThemeCapacities(themeDto, capacityTypes);
        return themeDto;
    }

    public CurriculumVersionOccupationModuleThemeDto createTheme(CurriculumVersionOccupationModuleThemeDto dto) {
        CurriculumVersionOccupationModuleTheme theme = new CurriculumVersionOccupationModuleTheme();
        CurriculumVersionOccupationModule module = em.getReference(CurriculumVersionOccupationModule.class, dto.getModule());
        theme.setModule(module);
        module.getThemes().add(theme);
        return updateTheme(theme, dto);
    }

    public CurriculumVersionOccupationModuleThemeDto updateTheme(CurriculumVersionOccupationModuleTheme theme,
            CurriculumVersionOccupationModuleThemeDto dto) {

        EntityUtil.bindToEntity(dto, theme, classifierRepository, "capacities", "outcomes");

        if(!ClassifierUtil.equals(VocationalGradeType.KUTSEHINDAMISVIIS_E, theme.getAssessment())) {
            theme.setGrade3Description(null);
            theme.setGrade4Description(null);
            theme.setGrade5Description(null);
        } 
        if(!ClassifierUtil.equals(VocationalGradeType.KUTSEHINDAMISVIIS_M, theme.getAssessment())) {
            theme.setPassDescription(null);
        }
        if(theme.getAssessment() == null) {
            theme.setTotalGradeDescription(null);
        }

        // check studyYearNumber and clear, if out of range
        int studyYears = CurriculumUtil.studyYears(theme.getModule().getCurriculumVersion().getCurriculum());
        if(theme.getStudyYearNumber() != null && theme.getStudyYearNumber().intValue() > studyYears) {
            theme.setStudyYearNumber(null);
        }

        updateThemeCapacities(theme, dto.getCapacities());
        updateThemeOutcomes(theme, dto.getOutcomes());

        CurriculumVersionOccupationModuleTheme saved = EntityUtil.save(theme, em);

        OccupationModuleCapacitiesUtil.updateModuleCapacities(saved.getModule(), 
                getCapacityTypes(EntityUtil.getId(theme.getModule().getCurriculumVersion().getCurriculum().getSchool())));
        OccupationModuleCapacitiesUtil.updateModuleYearCapacitiesHours(saved.getModule());
        EntityUtil.save(saved.getModule(), em);

        return getTheme(saved);
    }

    private void updateThemeCapacities(CurriculumVersionOccupationModuleTheme theme, 
            Set<CurriculumVersionOccupationModuleThemeCapacityDto> capactities) {
        Set<CurriculumVersionOccupationModuleThemeCapacityDto> filtered = capactities.stream().filter(c -> c.getHours() != null).collect(Collectors.toSet());
        EntityUtil.bindEntityCollection(theme.getCapacities(), CurriculumVersionOccupationModuleThemeCapacity::getId, 
                filtered, CurriculumVersionOccupationModuleThemeCapacityDto::getId, 
                this::createThemeCapacity, this::updateThemeCapacity);
    }

    private CurriculumVersionOccupationModuleThemeCapacity createThemeCapacity(CurriculumVersionOccupationModuleThemeCapacityDto dto) {
        CurriculumVersionOccupationModuleThemeCapacity capacity = new CurriculumVersionOccupationModuleThemeCapacity();
        return updateThemeCapacity(dto, capacity);
    }

    private CurriculumVersionOccupationModuleThemeCapacity updateThemeCapacity
    (CurriculumVersionOccupationModuleThemeCapacityDto dto, CurriculumVersionOccupationModuleThemeCapacity capacity) {
        EntityUtil.bindToEntity(dto, capacity, classifierRepository);
        return capacity;
    }

    public void deleteTheme(HoisUserDetails user, CurriculumVersionOccupationModuleTheme theme) {
        EntityUtil.setUsername(user.getUsername(), em);
        EntityUtil.deleteEntity(theme, em);

        OccupationModuleCapacitiesUtil.updateModuleCapacities(theme.getModule(), 
                getCapacityTypes(EntityUtil.getId(theme.getModule().getCurriculumVersion().getCurriculum().getSchool())));
        OccupationModuleCapacitiesUtil.updateModuleYearCapacitiesHours(theme.getModule());
        EntityUtil.save(theme.getModule(), em);
    }

    public List<CurriculumModuleOutcomeDto> themeOutcomes(CurriculumVersionOccupationModuleTheme theme) {
        List<?> data = em.createNativeQuery("select cvoo.curriculum_module_outcomes_id from curriculum_version_omodule_outcomes cvoo "
                + "where cvoo.curriculum_version_omodule_theme_id = ?1")
                .setParameter(1, EntityUtil.getId(theme))
                .getResultList();

        List<Long> outcomeIds = StreamUtil.toMappedList(r -> resultAsLong(r, 0), data);
        List<CurriculumModuleOutcomeDto> outcomeDtos = new ArrayList<>();

        if (!outcomeIds.isEmpty()) {
            List<CurriculumModuleOutcome> outcomes = em.createQuery("select cmo from CurriculumModuleOutcome cmo "
                    + "where cmo.id in (?1)", CurriculumModuleOutcome.class)
                    .setParameter(1, outcomeIds)
                    .getResultList();
            outcomeDtos = StreamUtil.toMappedList(o -> CurriculumModuleOutcomeDto.of(o), outcomes);
        }
        return outcomeDtos;
    }
}
