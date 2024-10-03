module reverse_string;

pure string reverseString(immutable string value) {
    char[] result = new char[value.length];
    auto idx = value.length;
    foreach (ch; value) 
        result[--idx] = ch;
    return result;
}

unittest
{
    immutable int allTestsEnabled = 0;

    // An empty string
    assert(reverseString("") == "");

    static if (allTestsEnabled)
    {
        // A word
        assert(reverseString("robot") == "tobor");

        // A capitalized word
        assert(reverseString("Ramen") == "nemaR");

        // A sentence with punctuation
        assert(reverseString("I'm hungry!") == "!yrgnuh m'I");

        // A palindrome
        assert(reverseString("racecar") == "racecar");

        // An even-sized word
        assert(reverseString("drawer") == "reward");
    }
}
