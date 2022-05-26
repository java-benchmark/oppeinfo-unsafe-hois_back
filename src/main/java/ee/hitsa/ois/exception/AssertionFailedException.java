package ee.hitsa.ois.exception;

public class AssertionFailedException extends RuntimeException {

    public AssertionFailedException() {
    }

    public AssertionFailedException(String message) {
        super(message);
    }

    public static void throwIf(boolean expression, String message) {
        if(expression) {
            throw new AssertionFailedException(message);
        }
    }
}
