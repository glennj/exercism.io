import java.util.function.UnaryOperator;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import java.util.stream.Stream;

class AffineCipher {
    private static final String ALPHABET = "abcdefghijklmnopqrstuvwxyz";
    private static final int ENCODED_GROUP_SIZE = 5;

    String encode(String text, int a, int b) {
        validateKey(a);

        String encoded = __code(text, c -> {
                int x = ALPHABET.indexOf(c);
                if (x != -1)
                    c = ALPHABET.charAt(
                            Math.floorMod(
                                    a * x + b,
                                    ALPHABET.length()
                            )
                    );
                return c;
        });
        return groupsOf(ENCODED_GROUP_SIZE, encoded);
    }

    String decode(String text, int a, int b) {
        validateKey(a);
        int a_inv = mmi(a, ALPHABET.length());

        return __code(text, c -> {
                int y = ALPHABET.indexOf(c);
                if (y != -1)
                    c = ALPHABET.charAt(
                            Math.floorMod(
                                    a_inv * (y - b),
                                    ALPHABET.length()
                            )
                    );
                return c;
        });
    }

    private void validateKey(int a) {
        if (gcd(a, ALPHABET.length()) != 1)
            throw new IllegalArgumentException("Error: keyA and alphabet size must be coprime.");
    }

    private String __code(String text, UnaryOperator<Character> op) {
        return text
                .replaceAll("[^\\p{Alnum}]", "")
                .toLowerCase()
                .chars()
                .mapToObj(c -> op.apply((char) c))
                .collect(StringBuilder::new, StringBuilder::append, StringBuilder::append)
                .toString();
    }

    private String groupsOf(int n, String text) {
        Stream.Builder<String> sb = Stream.builder();
        Matcher m = Pattern.compile(".{1," + n + "}").matcher(text);
        while (m.find())
            sb.accept(m.group());
        return sb.build().collect(Collectors.joining(" "));
    }

    private int mmi(int a, int m) {
        // not too efficient
        int n = 0;
        while (Math.floorMod(a * n, m) != 1)
            n++;
        return n;
    }

    private int gcd(int a, int b) {
        return b == 0 ? a : gcd(b, a % b);
    }
}
