package ee.hitsa.ois.util;

import java.math.BigDecimal;
import java.util.Comparator;
import java.util.Set;

import ee.hitsa.ois.domain.Classifier;
import ee.hitsa.ois.domain.curriculum.Curriculum;
import ee.hitsa.ois.domain.curriculum.CurriculumModule;
import ee.hitsa.ois.domain.curriculum.CurriculumOccupation;
import ee.hitsa.ois.domain.curriculum.CurriculumVersion;
import ee.hitsa.ois.domain.curriculum.CurriculumVersionOccupationModule;
import ee.hitsa.ois.enums.CurriculumDraft;
import ee.hitsa.ois.enums.CurriculumEhisStatus;
import ee.hitsa.ois.enums.CurriculumModuleType;
import ee.hitsa.ois.enums.CurriculumStatus;
import ee.hitsa.ois.enums.CurriculumVersionStatus;
import ee.hitsa.ois.enums.HigherModuleType;
import ee.hitsa.ois.enums.Language;
import ee.hitsa.ois.enums.MainClassCode;
import ee.hitsa.ois.enums.Permission;
import ee.hitsa.ois.enums.PermissionObject;
import ee.hitsa.ois.service.security.HoisUserDetails;
import ee.hitsa.ois.validation.ValidationFailedException;
import ee.hitsa.ois.web.dto.curriculum.CurriculumVersionHigherModuleDto;

public abstract class CurriculumUtil {

    public static final BigDecimal HOURS_PER_EKAP = BigDecimal.valueOf(26);

    public static boolean isHigher(Curriculum curriculum) {
        return Boolean.TRUE.equals(curriculum.getHigher());
    }

    public static boolean isVocational(Curriculum curriculum) {
        return Boolean.FALSE.equals(curriculum.getHigher());
    }

    public static boolean isCurriculumVersionConfirmed(CurriculumVersion version) {
        return ClassifierUtil.equals(CurriculumVersionStatus.OPPEKAVA_VERSIOON_STAATUS_K, version.getStatus());
    }
    
    public static String curriculumName(String curriculumCode, String curriculumName) {
        return curriculumCode + " - " + curriculumName;
    }
    
    public static String curriculumName(String merCode, String code, String name) {
        StringBuilder result = new StringBuilder();
        if (merCode != null) {
            result.append(merCode).append(" - ");
        }
        result.append(name).append(" (").append(code).append(")");
        return result.toString();
    }

    public static String curriculumNameWithMerCode(String curriculumCode, String curriculumName, String merCode) {
        String name = "";
        if (merCode != null) {
            name += merCode + " - ";
        }
        return name + curriculumName + " (" + curriculumCode + ")";
    }

    public static String moduleName(String moduleName, String moduleClassifierName) {
        return moduleName + " - " + moduleClassifierName;
    }

    public static String moduleName(String moduleName, String moduleClassifierName, String curriculumCode) {
        String name = moduleName(moduleName, moduleClassifierName);
        if (curriculumCode != null) {
            name += " (" + curriculumCode + ")";
        }
        return name;
    }

    public static String versionName(String versionCode, String curriculumName) {
        if (versionCode == null && curriculumName == null) return null;
        return versionCode + " " + curriculumName;
    }

    public static int studyYears(Curriculum c) {
        return studyYears(c.getStudyPeriod());
    }

    public static int studyYears(Integer studyMonths) {
        return studyMonths != null ? (studyMonths.intValue() + 11) / 12 : 1;
    }

    public static boolean basicDataCanBeEdited(Curriculum c) {
        return !ClassifierUtil.equals(CurriculumStatus.OPPEKAVA_STAATUS_K, c.getStatus());
    }

    public static boolean sameOrJointSchool(HoisUserDetails user, String userEhisShool, Curriculum curriculum) {
        if(UserUtil.isSameSchool(user, curriculum.getSchool())) {
            return true;
        }
        return userEhisShool != null && Boolean.TRUE.equals(curriculum.getJoint()) && curriculum.getJointPartners().stream()
                .anyMatch(p -> userEhisShool.equals(EntityUtil.getNullableCode(p.getEhisSchool())));
    }

    public static boolean occupationCanBeChanged(Classifier draft) {
        return !ClassifierUtil.equals(CurriculumDraft.OPPEKAVA_LOOMISE_VIIS_RIIKLIK, draft);
    }
    
    public static boolean occupationCanBeChanged(HoisUserDetails user, Classifier draft) {
        return occupationCanBeChanged(draft) || user.isSchoolAdmin();
    }

    private static boolean occupationCanBeDeleted(CurriculumOccupation occupation) {
        return !occupation.getCurriculum().getModules().stream()
                .flatMap(mod -> mod.getOccupations().stream())
                .map(occ -> occ.getOccupation())
                .filter(occ -> occupation.getOccupation().getCode().equals(occ.getCode()))
                .findAny().isPresent();
    }

    public static boolean isFreeModule(CurriculumModule m) {
        return ClassifierUtil.equals(CurriculumModuleType.KUTSEMOODUL_V, m.getModule());
    }
    
    public static int vocationalModuleOrderNr(String moduleCode) {
        if (moduleCode == null) {
            return 4;
        }
        int orderNr;
        if (moduleCode.equals(CurriculumModuleType.KUTSEMOODUL_P.name())) {
            orderNr = 1;
        } else if (moduleCode.equals(CurriculumModuleType.KUTSEMOODUL_Y.name())) {
            orderNr = 2;
        } else if (moduleCode.equals(CurriculumModuleType.KUTSEMOODUL_V.name())) {
            orderNr = 3;
        } else {
            orderNr = 4;
        }
        return orderNr;
    }

    public static int vocationalModuleOrderNr(CurriculumModule module) {
        return vocationalModuleOrderNr(EntityUtil.getCode(module.getModule()));
    }    

    public static int vocationalModuleOrderNr(CurriculumVersionOccupationModule occupationModule) {
        return vocationalModuleOrderNr(occupationModule.getCurriculumModule());
    }

    public static boolean canHaveOccupations(Curriculum curriculum) {
        return isVocational(curriculum) && 
                !ClassifierUtil.equals(CurriculumDraft.OPPEKAVA_LOOMISE_VIIS_TOOANDJA, curriculum.getDraft());
    }

    public static Comparator<CurriculumVersionHigherModuleDto> higherModuleComparator(Language lang) {
        return StreamUtil.comparingWithNullsLast(CurriculumVersionHigherModuleDto::getOrderNr)
                .thenComparing(m -> {
                        HigherModuleType type = EnumUtil.valueOf(HigherModuleType.class, m.getType());
                        return type != null ? type.getOrder() : null;
                    },  Comparator.nullsFirst(Comparator.naturalOrder()))
                .thenComparing(m -> Language.EN.equals(lang) ? m.getNameEn() : m.getNameEt(), String.CASE_INSENSITIVE_ORDER);
    }

    /**
     * Get part occupations of curriculum occupation
     * 
     * @return part occupations' codes
     */
    public static Set<String> getPartOccupationsCodes(CurriculumOccupation occupation) {
        return StreamUtil.toMappedSet(EntityUtil::getCode, getPartOccupationClassifiers(occupation));
    }

    public static Set<Classifier> getPartOccupationClassifiers(CurriculumOccupation occupation) {
        return StreamUtil.toMappedSet(c -> c.getClassifier(),  
                occupation.getOccupation().getChildConnects().stream()
                    .filter(c -> ClassifierUtil.mainClassCodeEquals(MainClassCode.OSAKUTSE, c.getClassifier())));
    }

//    User rights 

    /**
     * Checks user rights on curriculum search form: 
     * presence of edit/new buttons and filtering by status/ehis status depends on this method. 
     * In addition, it filters out unconfirmed curricula 
     * if user do not have correspondent role and permission.
     * 
     * Note, that user school is already considered in search method.
     */
    public static boolean canView(HoisUserDetails user) {
        return (user.isSchoolAdmin() || user.isLeadingTeacher())
                && UserUtil.hasPermission(user, Permission.OIGUS_V, PermissionObject.TEEMAOIGUS_OPPEKAVA);
    }

    /**
     * Checks user rights when opening curriculum or curriculum module form
     */
    public static boolean canView(HoisUserDetails user, String userEhisShool, Curriculum curriculum) {
        return ClassifierUtil.equals(CurriculumStatus.OPPEKAVA_STAATUS_K, curriculum.getStatus())
                || (user.isSchoolAdmin() || UserUtil.isLeadingTeacher(user, curriculum.getId()))
                        && UserUtil.hasPermission(user, Permission.OIGUS_V, PermissionObject.TEEMAOIGUS_OPPEKAVA)
                        && sameOrJointSchool(user, userEhisShool, curriculum);
    }

    public static boolean canView(HoisUserDetails user, String userEhisShool, CurriculumVersion version) {
        return isCurriculumVersionConfirmed(version)
                || (user.isSchoolAdmin() || UserUtil.isLeadingTeacher(user, EntityUtil.getId(version.getCurriculum())))
                        && UserUtil.hasPermission(user, Permission.OIGUS_V, PermissionObject.TEEMAOIGUS_OPPEKAVA)
                        && sameOrJointSchool(user, userEhisShool, version.getCurriculum());
    }

    public static boolean canCreate(HoisUserDetails user) {
        return user.isSchoolAdmin() && UserUtil.hasPermission(user, Permission.OIGUS_M, PermissionObject.TEEMAOIGUS_OPPEKAVA);
    }

    public static boolean canChange(HoisUserDetails user, String userEhisShool, Curriculum curriculum) {
        if(ClassifierUtil.equals(CurriculumStatus.OPPEKAVA_STAATUS_C, curriculum.getStatus())) {
            return false;
        }
        Permission permission = Permission.OIGUS_M;
        if(ClassifierUtil.oneOf(curriculum.getStatus(), CurriculumStatus.OPPEKAVA_STAATUS_M, CurriculumStatus.OPPEKAVA_STAATUS_K)) {
            permission = Permission.OIGUS_K;
        }
        return (user.isSchoolAdmin() || UserUtil.isLeadingTeacher(user, EntityUtil.getId(curriculum)))
                && UserUtil.hasPermission(user, permission, PermissionObject.TEEMAOIGUS_OPPEKAVA)
                && sameOrJointSchool(user, userEhisShool, curriculum);
    }

    public static boolean canConfirm(HoisUserDetails user, String userEhisShool, Curriculum curriculum) {
        return (user.isSchoolAdmin() || UserUtil.isLeadingTeacher(user, EntityUtil.getId(curriculum)))
                && UserUtil.hasPermission(user, Permission.OIGUS_K, PermissionObject.TEEMAOIGUS_OPPEKAVA)
                && sameOrJointSchool(user, userEhisShool, curriculum);
    }

    public static boolean canClose(HoisUserDetails user, String userEhisShool, Curriculum curriculum) {
        return canConfirm(user, userEhisShool, curriculum);
    }

    public static boolean canDelete(HoisUserDetails user, String userEhisShool, Curriculum curriculum) {
        return canChange(user, userEhisShool, curriculum);
    }

    /**
     * Method is intended to be used on curriculum search form. 
     * No need to check whether user is from the same or partner school, 
     * or that it is leading teacher's curriculum,
     * as only correspondent curricula appear in search results.
     */
    public static boolean canBeEdited(HoisUserDetails user, String status, String ehisStatus) {
        return (user.isSchoolAdmin() || user.isLeadingTeacher()) &&
                (CurriculumStatus.OPPEKAVA_STAATUS_S.name().equals(status) &&
                  UserUtil.hasPermission(user, Permission.OIGUS_M, PermissionObject.TEEMAOIGUS_OPPEKAVA)) || 
                (CurriculumStatus.OPPEKAVA_STAATUS_M.name().equals(status) &&
                  UserUtil.hasPermission(user, Permission.OIGUS_K, PermissionObject.TEEMAOIGUS_OPPEKAVA)) &&
                !CurriculumEhisStatus.OPPEKAVA_EHIS_STAATUS_A.name().equals(ehisStatus) &&
                !CurriculumEhisStatus.OPPEKAVA_EHIS_STAATUS_M.name().equals(ehisStatus);
    }

    public static boolean canSetUnderRevision(HoisUserDetails user, Curriculum curriculum) {
        return (UserUtil.isSchoolAdmin(user, curriculum.getSchool()) || UserUtil.isLeadingTeacher(user, EntityUtil.getId(curriculum)))
                && UserUtil.hasPermission(user, Permission.OIGUS_K, PermissionObject.TEEMAOIGUS_OPPEKAVA)
                && (ClassifierUtil.equals(CurriculumStatus.OPPEKAVA_STAATUS_K, curriculum.getStatus())
                        || ClassifierUtil.equals(CurriculumStatus.OPPEKAVA_STAATUS_M, curriculum.getStatus()));
    }

    public static boolean canSetUnderRevision(HoisUserDetails user, CurriculumVersion version) {
        return (UserUtil.isSchoolAdmin(user, version.getCurriculum().getSchool())
                || UserUtil.isLeadingTeacher(user, EntityUtil.getId(version.getCurriculum())))
                && UserUtil.hasPermission(user, Permission.OIGUS_K, PermissionObject.TEEMAOIGUS_OPPEKAVA)
                && ClassifierUtil.equals(CurriculumVersionStatus.OPPEKAVA_VERSIOON_STAATUS_K, version.getStatus());
    }
    
    public static boolean isMagisterOrDoctoralOrIntegratedStudy(Curriculum curriculum) {
        return isIntegratedStudy(curriculum) || isMagisterStudy(curriculum) || isDoctoralStudy(curriculum);
    }

    public static boolean isIntegratedStudy(Curriculum curriculum) {
        String studyLevel = EntityUtil.getCode(curriculum.getOrigStudyLevel());
        return studyLevel.matches("OPPEASTE_503");
    }
    
    public static boolean isMagisterStudy(Curriculum curriculum) {
        String studyLevel = EntityUtil.getCode(curriculum.getOrigStudyLevel());
        return studyLevel.matches("OPPEASTE_6.*?");
    }
    
    public static boolean isDoctoralStudy(Curriculum curriculum) {
        String studyLevel = EntityUtil.getCode(curriculum.getOrigStudyLevel());
        return studyLevel.matches("OPPEASTE_7.*?");
    }

//  User rights validation

    public static void assertCanView(HoisUserDetails user, String userEhisShool, CurriculumVersion version) {
        if(!canView(user, userEhisShool, version)) {
            throw new ValidationFailedException("main.messages.error.nopermission");
        }
    }

    public static void assertCanView(HoisUserDetails user, String userEhisShool, Curriculum curriculum) {
        if(!canView(user, userEhisShool, curriculum)) {
            throw new ValidationFailedException("main.messages.error.nopermission");
        }
    }

    public static void assertCanCreate(HoisUserDetails user) {
        if(!canCreate(user)) {
            throw new ValidationFailedException("main.messages.error.nopermission");
        }
    }

    public static void assertCanChange(HoisUserDetails user, String userEhisShool, Curriculum curriculum) {
        if(!canChange(user, userEhisShool, curriculum)) {
            throw new ValidationFailedException("main.messages.error.nopermission");
        }
    }

    public static void assertCanConfirm(HoisUserDetails user, String userEhisShool, Curriculum curriculum) {
        if(!canConfirm(user, userEhisShool, curriculum)) {
            throw new ValidationFailedException("main.messages.error.nopermission");
        }
    }

    public static void assertCanClose(HoisUserDetails user, String userEhisShool, Curriculum curriculum) {
        if(!canClose(user, userEhisShool, curriculum)) {
            throw new ValidationFailedException("main.messages.error.nopermission");
        }
    }

    public static void assertCanDelete(HoisUserDetails user, String userEhisShool, Curriculum curriculum) {
        if(!canDelete(user, userEhisShool, curriculum)) {
            throw new ValidationFailedException("main.messages.error.nopermission");
        }
    }

    public static void assertBasicDataCanBeEdited(Curriculum curriculum) {
        if(!basicDataCanBeEdited(curriculum)) {
            throw new ValidationFailedException("main.messages.error.nopermission");
        }
    }

    public static void assertOccupationCanBeChanged(Classifier draft) {
        if(!occupationCanBeChanged(draft)) {
            throw new ValidationFailedException("main.messages.error.nopermission");
        }
    }

    public static void assertOccupationCanBeDeleted(CurriculumOccupation occupation) {
        if(!occupationCanBeDeleted(occupation)) {
            throw new ValidationFailedException("main.messages.error.nopermission");
        }
    }
    public static void assertCanSetUnderRevision(HoisUserDetails user, Curriculum curriculum) {
        if (!canSetUnderRevision(user, curriculum)) {
            throw new ValidationFailedException("main.messages.error.nopermission");
        }
    }

    public static void assertCanSetUnderRevision(HoisUserDetails user, CurriculumVersion version) {
        if (!canSetUnderRevision(user, version)) {
            throw new ValidationFailedException("main.messages.error.nopermission");
        }
    }
}
