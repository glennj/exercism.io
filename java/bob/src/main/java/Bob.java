import java.util.regex.Pattern;

public class Bob {
    // shouting: at least one letter, but no lowercase letters.
    private static final Pattern SHOUTING = Pattern.compile("^(?=.*\\p{Alpha})(?!.*\\p{Lower})");

    public String hey(String input) {
        input = input.trim();

        boolean isQuestion = input.endsWith("?");
        boolean isSilent   = input.isEmpty();
        boolean isShouting = SHOUTING.matcher(input).find();

        if (isQuestion && isShouting)
            return "Calm down, I know what I'm doing!";
        else if (isQuestion)
            return "Sure.";
        else if (isShouting)
            return "Whoa, chill out!";
        else if (isSilent)
            return "Fine. Be that way!";
        else
            return "Whatever.";
    }
}
