module gigasecond;

import std.datetime;
import core.time : seconds;

private immutable auto gigaSecond = 1_000_000_000.seconds;

// Note to self: read the opBinary bits in the API docs.
pure DateTime add(DateTime dt) => dt + gigaSecond;


unittest
{
    immutable int allTestsEnabled = 1;

    // Date only specification of time
    assert(add(DateTime(2011, 4, 25)) == DateTime(2043, 1, 1, 1, 46, 40));

    static if (allTestsEnabled)
    {
        // Second test for date only specification of time"
        assert(add(DateTime(1977, 6, 13)) == DateTime(2009, 2, 19, 1, 46, 40));

        // Third test for date only specification of time
        assert(add(DateTime(1959, 7, 19)) == DateTime(1991, 3, 27, 1, 46, 40));

        // Full time specified
        assert(add(DateTime(2015, 1, 24, 22, 0, 0)) == DateTime(2046, 10, 2, 23, 46, 40));

        // Full time with day roll-over
        assert(add(DateTime(2015, 1, 24, 23, 59, 59)) == DateTime(2046, 10, 3, 1, 46, 39));

        // Does not mutate the input
        auto d = DateTime(2015, 1, 24, 23, 59, 59);
        assert(add(d) == DateTime(2046, 10, 3, 1, 46, 39));
        assert(d == DateTime(2015, 1, 24, 23, 59, 59));
    }

}
