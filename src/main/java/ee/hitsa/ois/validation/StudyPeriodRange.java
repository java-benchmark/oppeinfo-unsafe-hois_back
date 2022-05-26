package ee.hitsa.ois.validation;

import java.lang.annotation.ElementType;
import java.lang.annotation.Repeatable;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

import javax.validation.Constraint;
import javax.validation.Payload;

@Constraint(validatedBy = StudyPeriodRangeValidator.class)
@Target(ElementType.TYPE)
@Retention(RetentionPolicy.RUNTIME)
@Repeatable(StudyPeriodRanges.class)
public @interface StudyPeriodRange {
    String from() default "studyPeriodStart";
    String thru() default "studyPeriodEnd";

    String message() default "InvalidStudyperiodRange";
    Class<?>[] groups() default {};
    Class<? extends Payload>[] payload() default {};
}
