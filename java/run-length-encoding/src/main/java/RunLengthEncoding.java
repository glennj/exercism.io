import java.util.Collections;
import java.util.Spliterators;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.regex.*;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import java.util.stream.StreamSupport;

public class RunLengthEncoding {

    private static final Pattern P_ENCODE = Pattern.compile("(?<char>.)\\1*");

    public String encode(String phrase) throws IllegalArgumentException {
        if (phrase.matches(".*\\d.*"))
            throw new IllegalArgumentException("input cannot contain digits");

        return __code(phrase, P_ENCODE, matcher -> {
            int runLength = matcher.group().length();
            if (runLength == 1)
                return matcher.group("char");
            else
                return String.format("%d%s", runLength, matcher.group("char"));
        });
    }

    private static final Pattern P_DECODE = Pattern.compile("(?<digits>\\d+)(?<char>\\D)");
    private static final Pattern P_SINGLE_CHAR = Pattern.compile("(?:^|(?<=\\D))(?<char>\\D)");

    public String decode(String code) {
        // for encodings of run length 1, insert digit "1"
        code = P_SINGLE_CHAR.matcher(code).replaceAll("1${char}");

        return __code(code, P_DECODE, matcher -> {
            int len = Integer.parseInt(matcher.group("digits"));
            return String.join("", Collections.nCopies(len, matcher.group("char")));
        });
    }

    /* encode/decode engine:
     * - stream the input string into a MatchItr spliterator
     * - this will create a new stream of the pattern's matches
     * - the encode and decode methods can then manipulate the match groups.
     */
    private String __code(String string, Pattern pattern, Function<Matcher, String> coder) {
        return Stream.of(string)
                .flatMap(str -> StreamSupport.stream(new MatchItr(pattern.matcher(str)), false))
                .map(coder::apply)
                .collect(Collectors.joining());
    }

    // https://stackoverflow.com/a/28150956/7552
    private class MatchItr extends Spliterators.AbstractSpliterator<Matcher> {
        private final Matcher matcher;
        MatchItr(Matcher m) {
            super(m.regionEnd() - m.regionStart(), ORDERED | NONNULL);
            matcher = m;
        }
        @Override
        public boolean tryAdvance(Consumer<? super Matcher> action) {
            if (matcher.find()) {
                // for each match, return the Matcher.
                action.accept(matcher);
                return true;
            }
            return false;
        }
    }
}

/* iterative version
    public String encode(String phrase) {
        StringBuilder sb = new StringBuilder();
        Matcher m = P_ENCODE.matcher(phrase);
        while (m.find()) {
            int runLength = m.group().length();
            if (runLength > 1) sb.append(runLength);
            sb.append(m.group().charAt(0));
        }
        return sb.toString();
    }

    public String decode(String code) {
        // for encodings of run length 1, insert digit "1"
        code = P_SINGLE_CHAR.matcher(code).replaceAll("1${char}");
        StringBuilder sb = new StringBuilder();
        Matcher m = P_DECODE.matcher(code);
        while (m.find()) {
            int len = Integer.parseInt(m.group("digits"));
            String run = String.join("", Collections.nCopies(len, m.group("char")));
            sb.append(run);
        }
        return sb.toString();
    }
*/
