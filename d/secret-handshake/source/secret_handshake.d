module secret_handshake;

import std.algorithm : reverse;

private immutable auto actions = ["wink", "double blink", "close your eyes", "jump"];

pure string[] commands(immutable int number) {
    string[] result;
    foreach (idx, action; actions)
        if (number.bitTest(idx))
            result ~= action;
    return number.bitTest(actions.length) ? result.reverse : result;
}

pure bool bitTest(immutable int number, ulong position) =>
    ((number >> position) & 1) == 1;


unittest
{
    immutable int allTestsEnabled = 0;

    // Wink for 1
    assert(commands(1) == ["wink"]);

    static if (allTestsEnabled)
    {
        // Double blink for 10
        assert(commands(2) == ["double blink"]);

        // Close your eyes for 100
        assert(commands(4) == ["close your eyes"]);

        // Jump for 1000
        assert(commands(8) == ["jump"]);

        // Combine two actions
        assert(commands(3) == ["wink", "double blink"]);

        // Reverse two actions
        assert(commands(19) == ["double blink", "wink"]);

        // Reversing one action gives the same action
        assert(commands(24) == ["jump"]);

        // Reversing no actions still gives no actions
        assert(commands(16) == []);

        // All possible actions
        assert(commands(15) == ["wink", "double blink", "close your eyes", "jump"]);

        // Reverse all possible actions
        assert(commands(31) == ["jump", "close your eyes", "double blink", "wink"]);

        // Do nothing for zero
        assert(commands(0) == []);
    }
}

