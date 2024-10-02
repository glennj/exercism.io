module bob;

import std.algorithm : all, any, endsWith;
import std.ascii : isAlpha, isUpper;
import std.string : stripRight;
import std.range : empty;

/**
The surly teenager responds to some input.
Params: input = the input string
Returns: the string response.
*/
pure @safe string hey(string phrase) {
    string trimmed = phrase.stripRight;
    if (trimmed.empty) return "Fine. Be that way!";

    const bool isQuestion = trimmed.endsWith("?");
    const bool isYelling = trimmed.any!isAlpha && trimmed.all!(c => c.isUpper || !c.isAlpha);

    if (isYelling && isQuestion) return "Calm down, I know what I'm doing!";
    if (isYelling) return "Whoa, chill out!";
    if (isQuestion) return "Sure.";
    return "Whatever.";
}

unittest
{
    immutable int allTestsEnabled = 1;

    // Stating something
    assert(hey("Tom-ay-to, tom-aaaah-to.") == "Whatever.");

    static if (allTestsEnabled)
    {
        // Shouting
        assert(hey("WATCH OUT!") == "Whoa, chill out!");

        // Shouting gibberish
        assert(hey("FCECDFCAAB") == "Whoa, chill out!");

        // Asking a question
        assert(hey("Does this cryogenic chamber make me look fat?") == "Sure.");

        // Asking a numeric question
        assert(hey("You are, what, like 15?") == "Sure.");

        // Asking gibberish
        assert(hey("fffbbcbeab?") == "Sure.");

        // Talking forcefully
        assert(hey("Hi there!") == "Whatever.");

        // Using acronyms in regular speech
        assert(hey("It's OK if you don't want to go work for NASA.") == "Whatever.");

        // Forceful question
        assert(hey("WHAT'S GOING ON?") == "Calm down, I know what I'm doing!");

        // Shouting numbers
        assert(hey("1, 2, 3 GO!") == "Whoa, chill out!");

        // No letters
        assert(hey("1, 2, 3") == "Whatever.");

        // Question with no letters
        assert(hey("4?") == "Sure.");

        // Shouting with special characters
        assert(hey("ZOMG THE %^*@#$(*^ ZOMBIES ARE COMING!!11!!1!") == "Whoa, chill out!");

        // Shouting with no exclamation mark
        assert(hey("I HATE THE DENTIST") == "Whoa, chill out!");

        // Statement containing question mark
        assert(hey("Ending with a ? means a question.") == "Whatever.");

        // Non-letters with question
        assert(hey(":) ?") == "Sure.");

        // Prattling on
        assert(hey("Wait! Hang on. Are you going to be OK?") == "Sure.");

        // Silence
        assert(hey("") == "Fine. Be that way!");

        // Prolonged silence
        assert(hey("          ") == "Fine. Be that way!");

        // Alternate silence
        assert(hey("\t\t\t\t\t\t\t\t\t\t") == "Fine. Be that way!");

        // Multiple line question
        assert(hey("\nDoes this cryogenic chamber make me look fat?\nNo.") == "Whatever.");

        // Ending with whitespace
        assert(hey("Okay if like my  spacebar  quite a bit?   ") == "Sure.");

        // Other whitespace
        assert(hey("\n\r \t") == "Fine. Be that way!");

        // Non-question ending with whitespace
        assert(hey("This is a statement ending with whitespace      ") == "Whatever.");
    }

}
