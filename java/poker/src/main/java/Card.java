import java.util.Arrays;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

class Card {
    static final List<String> FACES = Arrays.asList(
            "2", "3", "4", "5", "6", "7", "8", "9", "10", "J", "Q", "K", "A"
    );
    private final int value;
    private final String suit;
    private static final Pattern PATT = Pattern.compile("^([2-9]|10|[JQKA])([CSHD])$");

    Card(String input) {
        Matcher m = PATT.matcher(input);
        if (!m.find())
            throw new IllegalArgumentException("Invalid card: " + input);
        value = FACES.indexOf(m.group(1));
        suit = m.group(2);
    }

    int getValue()   { return value; }
    String getSuit() { return suit; }

    @Override
    public String toString() {
        return FACES.get(value) + suit;
    }
}
