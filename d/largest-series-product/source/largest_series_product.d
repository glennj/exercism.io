module largest_series_product;

import std.algorithm : all, fold, map, maxElement;
import std.ascii : isDigit;
import std.exception : enforce;
import std.range : iota;

pure ulong largestProduct(immutable string digits, int span) {
    enforce(span >= 0, "Span too short");
    enforce(span <= digits.length, "Span too long");
    enforce(digits.all!isDigit, "Non-digits detected");

    ulong product(string digitStr) => digitStr.fold!((p, c) => p * (c - '0'))(1);

    return (digits.length - span + 1).iota
        .map!((idx) => product(digits[idx .. idx + span]))
        .maxElement;
}


unittest
{
    import std.exception : assertThrown;

    immutable int allTestsEnabled = 1;

    // Finds the largest product if span equals length
    assert(largestProduct("29", 2) == 18);

    static if (allTestsEnabled)
    {
        // Can find the largest product of 2 with numbers in order
        assert(largestProduct("0123456789", 2) == 72);

        // Can find the largest product of 2
        assert(largestProduct("576802143", 2) == 48);

        // Can find the largest product of 3 with numbers in order
        assert(largestProduct("0123456789", 3) == 504);

        // Can find the largest product of 3
        assert(largestProduct("1027839564", 3) == 270);

        // Can find the largest product of 5 with numbers in order
        assert(largestProduct("0123456789", 5) == 15120);

        // Can get the largest product of a big number
        assert(largestProduct("73167176531330624919225119674426574742355349194934", 6) == 23520);

        // Reports zero if the only digits are zero
        assert(largestProduct("0000", 2) == 0);

        // Reports zero if all spans include zero
        assert(largestProduct("99099", 3) == 0);

        // Rejects span longer than string length
        assertThrown(largestProduct("123", 4));

        // Reports 1 for empty string and empty product (0 span)
        assert(largestProduct("", 0) == 1);

        // Reports 1 for nonempty string and empty product (0 span)
        assert(largestProduct("123", 0) == 1);

        // Rejects empty string and nonzero span
        assertThrown(largestProduct("", 1));

        // Rejects invalid character in digits
        assertThrown(largestProduct("1234a5", 2));

        // Rejects negative span
        assertThrown(largestProduct("12345", -1));
    }
}
