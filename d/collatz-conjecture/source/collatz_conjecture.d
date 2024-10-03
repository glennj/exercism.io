module collatz_conjecture;

import std.exception : enforce;

pure int steps(immutable int number, immutable int step = 0) {
    enforce(number > 0, "Number must be greater than zero.");

    if (number == 1) 
        return step;
    if (number % 2 == 0)
        return steps(number / 2, step + 1);
    else
        return steps(3 * number + 1, step + 1);
}

unittest
{
    import std.exception : assertThrown;

    const int allTestsEnabled = 1;

    // Zero steps for one
    assert(steps(1) == 0);

    static if (allTestsEnabled)
    {
        // Divide if even
        assert(steps(16) == 4);

        // Even and odd steps
        assert(steps(12) == 9);

        // Large number of even and odd steps
        assert(steps(1000000) == 152);

        // Zero is an error
        assertThrown(steps(0));

        // Negative value is an error
        assertThrown(steps(-15));
    }
}
