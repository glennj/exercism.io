module rotational_cipher;

import std.conv : to;
import std.range : chain, dropExactly, takeExactly;
import std.ascii : toUpper;

pure string rotate(immutable string text, immutable int shiftKey) {
    string alpha = "abcdefghijklmnopqrstuvwxyz";
    string rotated = chain(
        alpha.dropExactly(shiftKey),
        alpha.takeExactly(shiftKey),
    ).to!string;

    char[char] map;
    foreach (i, c; alpha) {
        map[c] = rotated[i];
        map[c.toUpper] = rotated[i].toUpper;
    }

    string result = "";
    foreach (c; text)
        result ~= c in map ? map[c] : c;

    return result;
}


unittest
{

    immutable int allTestsEnabled = 1;

    // rotate a by 0, same output as input
    {
        assert("a" == rotate("a", 0));
    }

    static if (allTestsEnabled)
    {
        // rotate a by 1
        {
            assert("b" == rotate("a", 1));
        }

        // rotate a by 26, same output as input
        {
            assert("a" == rotate("a", 26));
        }

        // rotate m by 13
        {
            assert("z" == rotate("m", 13));
        }

        // rotate n by 13 with wrap around alphabet
        {
            assert("a" == rotate("n", 13));
        }

        // rotate capital letters
        {
            assert("TRL" == rotate("OMG", 5));
        }

        // rotate spaces
        {
            assert("T R L" == rotate("O M G", 5));
        }

        // rotate numbers
        {
            assert("Xiwxmrk 1 2 3 xiwxmrk" == rotate("Testing 1 2 3 testing", 4));
        }

        // rotate punctuation
        {
            assert("Gzo'n zvo, Bmviyhv!" == rotate("Let's eat, Grandma!", 21));
        }

        // rotate all letters
        {
            immutable text = "The quick brown fox jumps over the lazy dog.";
            immutable shiftKey = 13;
            immutable expected = "Gur dhvpx oebja sbk whzcf bire gur ynml qbt.";
            assert(expected == rotate(text, shiftKey));
        }
    }
}
