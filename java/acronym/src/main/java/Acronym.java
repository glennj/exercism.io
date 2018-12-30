import java.util.regex.*;
import static java.util.Objects.requireNonNull;

class Acronym {

    // This is a letter at the start of the string,
    // or preceded by whitespace or a hyphen.
    private final static Pattern PATT = Pattern.compile("(?<=^|\\s|-)(\\p{Alpha})");

    private final String acronym;

    Acronym(String phrase) {
        requireNonNull(phrase);

        StringBuffer sb = new StringBuffer();
        Matcher m = PATT.matcher(phrase);
        while (m.find()) {
            sb.append(m.group(1));
        }
        acronym = sb.toString().toUpperCase();
    }

    String getAcronym() {
        return acronym;
    }
}
