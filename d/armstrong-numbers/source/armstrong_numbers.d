module armstrong_numbers;

import std.conv : to;
import std.math : log10, pow, trunc;

pure @safe bool isArmstrongNumber(int number) {
    assert(number >= 0);
    if (number == 0) return true;

    int len = number.intLength, num = number, sum = 0;

    while (num > 0) {
        sum += (num % 10).pow(len);
        num /= 10;
    }

    return sum == number;
}

private pure @safe int intLength(int num) => 1 + num.to!float.log10.trunc.to!int;

unittest {
    immutable int allTestsEnabled = 1;

    // Zero is an Armstrong number
    assert(isArmstrongNumber(0));

    static if (allTestsEnabled) {
        // Single digit numbers are Armstrong numbers
        assert(isArmstrongNumber(5));

        // There are no 2 digit Armstrong numbers
        assert(!isArmstrongNumber(10));

        // Three digit number that is an Armstrong number
        assert(isArmstrongNumber(153));

        // Three digit number that is not an Armstrong number
        assert(!isArmstrongNumber(100));

        // Four digit number that is an Armstrong number
        assert(isArmstrongNumber(9474));

        // Four digit number that is not an Armstrong number
        assert(!isArmstrongNumber(9475));

        // Seven digit number that is an Armstrong number
        assert(isArmstrongNumber(9_926_315));

        // Seven digit number that is not an Armstrong number
        assert(!isArmstrongNumber(9_926_314));
    }
}
