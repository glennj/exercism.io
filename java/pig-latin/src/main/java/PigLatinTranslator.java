import java.util.Arrays;
import java.util.List;
import java.util.regex.*;
import java.util.stream.Collectors;

public class PigLatinTranslator {
    String translate(String phrase) {
        return Arrays
                .stream(phrase.split("\\s+"))
                .map(String::toLowerCase)
                .map(this::translateWord)
                .collect(Collectors.joining(" "));
    }

    // "apple" => "appleay", "xray" => "xrayay"
    private static Pattern NO_PREFIX = Pattern.compile("^(?:[aeiou]|xr|yt).*");

    private static List<Pattern> HAS_PREFIX = Arrays.asList(
            // 1. "square" => "aresquay", "quip" => "ipquay"
            Pattern.compile("^(.?qu)(.+)"),
            // 2. "rhythm" => "ythmrhay", "shyly" => "ylyshay"
            Pattern.compile("^([^aeiouy]+)(y.*)"),
            // 3. "strengths" => "engthsstray"
            Pattern.compile("^([^aeiou]+)(.*)")
    );

    private String translateWord(String word) {
        Matcher m = NO_PREFIX.matcher(word);
        if (m.matches())
            return word + "ay";

        for (Pattern p : HAS_PREFIX) {
            m = p.matcher(word);
            if (m.matches())
                return String.format("%s%say", m.group(2), m.group(1));
        }

        return word + "ay";
    }
}
