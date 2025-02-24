import java.util.regex.*

class PigLatin {

    static String translate(String phrase) {
        phrase.tokenize()
              .collect(this::translateWord)
              .join(" ")
    }

    static List<Pattern> patterns = [
        Pattern.compile("^()((?:xr|yt|[aeiou]).*)"),    // apple, xray, yttria
        Pattern.compile("^([^aeiou]*qu)(.*)"),          // quiz, squelch
        Pattern.compile("^([^aeiouy]+)(y.*)"),          // my, rhythm
        Pattern.compile("^([^aeiou]+)(.*)")             // least, specific
    ]

    static String translateWord(String phrase) {
        for (p in patterns) {
            def m = p.matcher(phrase)
            if (m.matches())
                return m.group(2) + m.group(1) + "ay"
        }

        return phrase
    }
}
