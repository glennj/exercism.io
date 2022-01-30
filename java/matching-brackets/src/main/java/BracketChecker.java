import java.util.HashMap;
import java.util.Map;
import java.util.Stack;
import java.util.function.IntPredicate;
import java.util.stream.IntStream;
import java.util.regex.Pattern;

interface IBracketChecker {
    boolean areBracketsMatchedAndNestedCorrectly();
}

public class BracketChecker {
    private IBracketChecker implementation;
    BracketChecker(String input) {
//        implementation = new StreamBased(input);
        implementation = new StringBased(input);
    }
    public boolean areBracketsMatchedAndNestedCorrectly() {
        return implementation.areBracketsMatchedAndNestedCorrectly();
    }

    /* A regex based solution inspired by veezex
     * https://exercism.io/tracks/java/exercises/bracket-push/solutions/5e53fc61cb0f4b71a6f83bc2559c2d72
     */
    private class StringBased implements IBracketChecker {
        private String bracketString;
        private Pattern NON_BRACKETS = Pattern.compile("[^(){}\\[\\]]");
        private Pattern BRACKET_PAIRS = Pattern.compile("\\(\\)|\\{\\}|\\[\\]");

        StringBased(String input) {
            bracketString = NON_BRACKETS.matcher(input).replaceAll("");
        }

        public boolean areBracketsMatchedAndNestedCorrectly() {
            while (true) {
                String reduced = BRACKET_PAIRS.matcher(bracketString).replaceAll("");
                if (reduced.equals(bracketString))  // no more matches
                    break;
                bracketString = reduced;
            }
            return bracketString.isEmpty();
        }
    }

    /* An implementation that is more stream- based, processing the brackets one at a time.
     * The final processing of the bracket stream is a bit of nightmare.
     */

    // no static members allowed in inner classes :(
    private static Map<Character, Character> BRACKETS = new HashMap<>();
    static {
        BRACKETS.put(')', '(');
        BRACKETS.put(']', '[');
        BRACKETS.put('}', '{');
    }
    interface VariableHolder {
        boolean isValid();
        void invalidate();
    }
    private class StreamBased implements IBracketChecker {
        private IntPredicate isOpenBracket = c -> BRACKETS.values().contains((char) c);
        private IntPredicate isCloseBracket = c -> BRACKETS.keySet().contains((char) c);
        private IntPredicate isBracket = c -> isOpenBracket.test(c) || isCloseBracket.test(c);

        private final IntStream bracketStream;

        StreamBased(String input) {
            bracketStream = input.chars().filter(isBracket);
        }

        public boolean areBracketsMatchedAndNestedCorrectly() {
            Stack<Character> stack = new Stack<>();
            VariableHolder h = new VariableHolder() {
                private boolean validity = true;
                public boolean isValid() { return validity; }
                public void invalidate() { validity = false; }
            };

            bracketStream
                    .forEach(bracket -> {
                        if (!h.isValid()) // ignore rest of stream
                            return;

                        if (isOpenBracket.test(bracket)) {
                            stack.push((char) bracket);
                        } else {
                            // A close bracket: can it be paired with its open bracket?
                            if (stack.empty() || stack.peek() != BRACKETS.get((char) bracket)) {
                                h.invalidate();
                            } else {
                                stack.pop();
                            }
                        }
                    });

            return h.isValid() && stack.empty();
        }
    }
}
