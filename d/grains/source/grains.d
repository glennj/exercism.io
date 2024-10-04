module grains;

//import std.algorithm : fold;
//import std.range : iota;
import std.bigint;
import std.conv : to;
import std.exception : enforce;

private immutable int maxSquare = 64;

pure ulong square(immutable ulong num) {
    enforce(1 <= num && num <= maxSquare, "Invalid square");
    return 1uL << (num - 1);
}

//pure ulong total() => maxSquare.iota.fold!((t, s) => t + square(s + 1))(0uL);
pure ulong total() => ((BigInt(1) << maxSquare) - 1).to!ulong;


unittest
{
    import std.exception : assertThrown;

    immutable int allTestsEnabled = 0;

    // Returns the number of grains on the square
    assert(square(1) == 1);

    static if (allTestsEnabled)
    {
        assert(square(2) == 2);
        assert(square(3) == 4);
        assert(square(4) == 8);
        assert(square(16) == 32_768);
        assert(square(32) == 2_147_483_648uL);
        assert(square(64) == 9_223_372_036_854_775_808uL);

        // Square 0 raises an exception
        assertThrown(square(0));

        // Negative square raises an exception
        assertThrown(square(-1));

        // Square greater than 64 raises an exception
        assertThrown(square(65));

        // Returns the total number of grains on the board
        assert(total() == 18_446_744_073_709_551_615uL);
    }
}
