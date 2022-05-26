package ee.hitsa.ois.validation;

import java.time.LocalDateTime;

import javax.validation.ConstraintValidator;
import javax.validation.ConstraintValidatorContext;

import org.springframework.beans.PropertyAccessor;
import org.springframework.beans.PropertyAccessorFactory;

public class DateTimeRangeValidator implements ConstraintValidator<DateTimeRange, Object> {

    private DateTimeRange constraint;

    @Override
    public void initialize(DateTimeRange constraintAnnotation) {
        this.constraint = constraintAnnotation;
    }

    @Override
    public boolean isValid(Object value, ConstraintValidatorContext context) {
        PropertyAccessor reader = PropertyAccessorFactory.forBeanPropertyAccess(value);
        LocalDateTime from = (LocalDateTime) reader.getPropertyValue(constraint.from());
        if(from == null) {
            return true;
        }
        LocalDateTime thru = (LocalDateTime)reader.getPropertyValue(constraint.thru());
        return thru == null || (!thru.isBefore(from) && (constraint.allowPast() || !from.isBefore(LocalDateTime.now())));
    }
}
