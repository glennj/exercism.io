module square_root;

import std.conv : to;
import std.math : log, pow, floor;

// Using the Binary numeral system implementation from
// https://en.wikipedia.org/wiki/Methods_of_computing_square_roots#Binary_numeral_system_(base_2)

pure int squareRoot(immutable int radicand) {
    int n = radicand;
    int root = 0;
    // find b, the greatest power of 4 less than or equal to n
    int b = 4.pow((log(n.to!double) / log(4.0)).to!int);

    while (b > 0) {
        if (n >= root + b) {
            n -= root + b;
            root = root / 2 + b;
        }
        else
            root /= 2;
        b /= 4;
    }
    return root;
}

unittest
{
    immutable int allTestsEnabled = 1;

    // root of 1
    assert(squareRoot(1) == 1);

    static if (allTestsEnabled)
    {
        // root of 4
        assert(squareRoot(4) == 2);

        // root of 25
        assert(squareRoot(25) == 5);

        // root of 81
        assert(squareRoot(81) == 9);

        // root of 196
        assert(squareRoot(196) == 14);

        // root of 65,025
        assert(squareRoot(65_025) == 255);
    }
}
