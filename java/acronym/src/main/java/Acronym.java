import java.util.regex.*;
import static java.util.Objects.requireNonNull;

class Acronym {

    private final String phrase;
    private String acronym;
    
    // This is a sequence of non-(space or hyphen) at the start of the string,
    // or preceded by whitespace or a hyphen.
    private final static Pattern PATT = Pattern.compile("(?<=^|\\s|-)[^\\s-]+");
    private final static Pattern LETTER = Pattern.compile("\\p{Alpha}");
    
    Acronym(String phrase) {
        requireNonNull(phrase);
        this.phrase = phrase;
    }

    String get() {
        if (this.acronym == null) {
            StringBuffer sb = new StringBuffer();
            Matcher m = PATT.matcher(this.phrase);
            while (m.find()) {
                String word = m.group();
                // get the first letter in the word:
                // the first char of the word may not be a letter;
                // there may be no letters in this word.
                Matcher l = LETTER.matcher(word);
                if (l.find()) {
                    sb.append(l.group());
                }
            }
            acronym = sb.toString().toUpperCase();
        }
        return acronym;
    }
}
