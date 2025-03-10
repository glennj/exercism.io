module isogram;

import std.ascii : isAlpha, toLower;

pure bool isIsogram(immutable string phrase) {
    int[char] count;
    foreach (c; phrase)
        if (c.isAlpha && ++count[c.toLower] > 1)
            return false;
    return true;
}
    

unittest
{
    immutable int allTestsEnabled = 0;

    // Empty string
    assert(isIsogram(""));

    static if (allTestsEnabled)
    {
        // Isogram with only lower case characters
        assert(isIsogram("isogram"));

        // Word with one duplicated character
        assert(!isIsogram("eleven"));

        // Word with one duplicated character from the end of the alphabet
        assert(!isIsogram("zzyzx"));

        // Longest reported english isogram
        assert(isIsogram("subdermatoglyphic"));

        // Word with duplicated character in mixed case
        assert(!isIsogram("Alphabet"));

        // Word with duplicated character in mixed case, lowercase first
        assert(!isIsogram("alphAbet"));

        // Hypothetical isogrammic word with hyphen
        assert(isIsogram("thumbscrew-japingly"));

        // Hypothetical word with duplicated character following hyphen
        assert(!isIsogram("thumbscrew-jappingly"));

        // Isogram with duplicated hyphen
        assert(isIsogram("six-year-old"));

        // Made-up name that is an isogram
        assert(isIsogram("Emily Jung Schwartzkopf"));

        // Duplicated character in the middle
        assert(!isIsogram("accentor"));

        // Same first and last characters
        assert(!isIsogram("angola"));
    }
}
