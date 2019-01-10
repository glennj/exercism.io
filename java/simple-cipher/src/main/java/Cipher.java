import java.util.Random;
import java.util.stream.IntStream;
import java.util.stream.Stream;

class Cipher {
    private static final int KEY_LENGTH = 100;
    private static final String ALPHABET = "abcdefghijklmnopqrstuvwxyz";
    private String key;

    Cipher() {
        key = randomKey();
    }

    Cipher(String key) {
        if (key.length() == 0)
            throw new IllegalArgumentException("Empty key");
        if (key.matches(".*[^a-z].*"))
            throw new IllegalArgumentException("Key must be only lower case letters");
        this.key = key;
    }

    String getKey() { return key; }

    String encode(String text) { return __code(text, +1); }
    String decode(String text) { return __code(text, -1); }

    private String __code(String text, int direction) {
        while (key.length() < text.length()) {
            key += key;
        }
        String lc = text.toLowerCase();
        return charStreamToString(
                IntStream
                        .range(0, text.length())
                        .mapToObj(i -> codedChar(lc, i, direction))
        );
    }

    private char codedChar(String text, int index, int direction) {
        int textIdx  = ALPHABET.indexOf(text.charAt(index));
        int keyIdx   = ALPHABET.indexOf(key.charAt(index));
        int codedIdx = Math.floorMod(
                textIdx + direction * keyIdx,
                ALPHABET.length()
        );
        return ALPHABET.charAt(codedIdx);
    }

    private String randomKey() {
        return charStreamToString(
                new Random()
                        .ints(KEY_LENGTH, 0, ALPHABET.length())
                        .mapToObj(i -> (char)('a' + i))
        );
    }

    private String charStreamToString(Stream<Character> stream) {
        return stream
                .collect(StringBuilder::new, StringBuilder::append, StringBuilder::append)
                .toString();
    }
}
