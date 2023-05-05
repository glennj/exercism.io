# Returns Bob's response to someone talking to him.
#
# + input - whatever is said to Bob
# + return - Bob's response
public function hey(string input) returns string {
    string trimmed = input.trim();
    if trimmed.length() == 0 {
        return "Fine. Be that way!";
    }

    boolean isQuestion = trimmed.endsWith("?");
    boolean isShouting = re `.*\p{Lu}.*`.isFullMatch(trimmed)
                    && !(re `.*\p{Ll}.*`.isFullMatch(trimmed));

    if isShouting && isQuestion {
        return "Calm down, I know what I'm doing!";
    } else if isShouting {
        return "Whoa, chill out!";
    } else if isQuestion {
        return "Sure.";
    } else {
        return "Whatever.";
    }
}
