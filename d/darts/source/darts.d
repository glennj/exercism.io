module darts;

import std.math : hypot;

pure int score(immutable float x, immutable float y)
{
    immutable float distance = hypot(x, y);
    if (distance <= 1) return 10;
    if (distance <= 5) return 5;
    if (distance <= 10) return 1;
    return 0;
}

unittest
{
    immutable int allTestsEnabled = 1;

    // Missed target
    assert(score(-9, 9) == 0);

    static if (allTestsEnabled)
    {
        // On the outer circle
        assert(score(0, 10) == 1);

        // On the middle circle
        assert(score(-5, 0) == 5);

        // On the inner circle
        assert(score(0, -1) == 10);

        // Exactly on centre
        assert(score(0, 0) == 10);

        // Near the centre
        assert(score(-0.1, -0.1) == 10);

        // Just within the inner circle
        assert(score(0.7, 0.7) == 10);

        // Just outside the inner circle
        assert(score(0.8, -0.8) == 5);

        // Just within the middle circle
        assert(score(-3.5, 3.5) == 5);

        // Just outside the middle circle
        assert(score(-3.6, -3.6) == 1);

        // Just within the outer circle
        assert(score(-7.0, 7.0) == 1);

        // Just outside the outer circle
        assert(score(7.1, -7.1) == 0);

        // Asymmetric position between the inner and middle circles
        assert(score(0.5, -4) == 5);
    }
}
