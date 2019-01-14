import java.util.HashMap;
import java.util.Map;
import java.util.function.IntBinaryOperator;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

class WordProblemSolver {
    private static final Map<String, IntBinaryOperator> calculator;
    static {
        calculator = new HashMap<>();
        calculator.put("plus",          (a, b) -> a + b);
        calculator.put("minus",         (a, b) -> a - b);
        calculator.put("multiplied by", (a, b) -> a * b);
        calculator.put("divided by",    (a, b) -> a / b);
    }

    private static final Pattern EXPR;
    static {
        String integer = "-?\\d+";
        String operator = calculator.keySet().stream().collect(Collectors.joining("|"));
        String pattern = String.format("(%s)\\s+(%s)\\s+(%s)", integer, operator, integer);
        EXPR = Pattern.compile(pattern);
    }

    public int solve(String question) {
        question = question.replaceFirst("^What is ", "").replaceFirst("\\?$", "");

        Matcher m = EXPR.matcher(question);
        while (m.find()) {
            int ans = calculator
                    .get(m.group(2))
                    .applyAsInt(
                            Integer.parseInt(m.group(1)), 
                            Integer.parseInt(m.group(3))
                    );
            question = m.replaceFirst(String.valueOf(ans));
            m = EXPR.matcher(question);
        }

        // at this point, we could have a unary operator dispatcher
        // to handle things like "-5 absolute value" or "53 cubed"

        try {
            return Integer.parseInt(question);
        } catch (NumberFormatException e) {
            throw new IllegalArgumentException("I'm sorry, I don't understand the question!");
        }
    }
}
