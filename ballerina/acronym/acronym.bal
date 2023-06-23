import ballerina/lang.regexp;

# Returns the acronym of the given phrase.
#
# + phrase - a string
# + return - the acronym
function abbreviate(string phrase) returns string {
    string capitalized = phrase.toUpperAscii();
    //return abbreviate_iterate(capitalized);
    return abbreviate_regex(capitalized);
}

// ======================================================
// Iterate over the phrase character-by-character,
// finding the letters that comrise the acronym.
function abbreviate_iterate(string phrase) returns string {
    string acr = "";
    boolean seekingAlpha = true;

    foreach int cp in phrase.toCodePointInts() {
        if seekingAlpha {
            if isLetter(cp) {
                string:Char|error ch = string:fromCodePointInt(cp);
                if ch is error {
                    panic(ch);
                }
                acr += ch;
                seekingAlpha = false;
            }
        } else {
            if !(isLetter(cp) || isApostrophe(cp)) {
                seekingAlpha = true;
            }
        }
    }

    return acr;
}

function isLetter(int codepoint) returns boolean {
    return (65 <= codepoint && codepoint <= 90);
}

function isApostrophe(int codepoint) returns boolean => codepoint == 39;

// ======================================================
// Use a regular expression to find all the "words" (starts
// with a letter, followed by letters and apostrophes), and
// construct the acronym with the initial letters.
function abbreviate_regex(string phrase) returns string {
    var textFromSpan = function(regexp:Span span) returns string {
        return phrase.substring(span.startIndex, span.endIndex);
    };
    var buildAcronym = function(string acr, string word) returns string {
        return acr + word[0];
    };
    string:RegExp r = re `[A-Z][A-Z']*`;

    return r.findAll(phrase)
            .'map(textFromSpan)
            .reduce(buildAcronym, "");

}
