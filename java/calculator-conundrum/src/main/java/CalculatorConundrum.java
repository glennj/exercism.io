import java.util.Objects;

class CalculatorConundrum {
    public String calculate(int operand1, int operand2, String operation) {
        if (Objects.isNull(operation)) {
            throw new IllegalArgumentException("Operation cannot be null");
        }
        if (operation.isEmpty()) {
            throw new IllegalArgumentException("Operation cannot be empty");
        }

        try {
            int answer = calculateAnswer(operand1, operand2, operation);
            return String.format("%d %s %d = %d", operand1, operation, operand2, answer);
        } catch (ArithmeticException e) {
            if (e.getMessage().startsWith("/ by zero"))
                throw new IllegalOperationException("Division by zero is not allowed", e);
            else
                throw e;
        } catch (Exception e) {
            throw e;
        }
    }

    private int calculateAnswer(int operand1, int operand2, String operation) {
        switch (operation) {
            case "+": return operand1 + operand2;
            case "-": return operand1 - operand2;
            case "*": return operand1 * operand2;
            case "/": return operand1 / operand2;
            default:
                String msg = "Operation '%s' does not exist".formatted(operation);
                throw new IllegalOperationException(msg);
        }
    }
}
