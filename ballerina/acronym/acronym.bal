# Returns the acronym of the given phrase.
#
# + phrase - a string
# + return - the acronym
function abbreviate(string phrase) returns string {
    string acr = "";
    boolean seekingAlpha = true;

    foreach int cp in phrase.toUpperAscii().toCodePointInts() {
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
