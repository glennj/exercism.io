module Raindrops;

import std.algorithm : filter, fold;
import std.conv : to;
import std.typecons : tuple;

private alias drop = tuple!("divisor", "sound");
private immutable auto drops = [drop(3, "Pling"), drop(5, "Plang"), drop(7, "Plong")];

pure string convert(immutable int n) {
//    string result;
//    foreach (d; drops) if (n % d.divisor == 0) result ~= d.sound;
    string result = drops
        .filter!((d) => n % d.divisor == 0)
        .fold!((s, d) => s ~ d.sound)("");
    return result.length ? result : n.to!string;
}


unittest
{
    immutable int allTestsEnabled = 1;

    // The sound for 1 is 1
    assert(convert(1) == "1");

    static if (allTestsEnabled)
    {
        // The sound for 3 is Pling
        assert(convert(3) == "Pling");

        // The sound for 5 is Plang
        assert(convert(5) == "Plang");

        // the sound for 7 is Plong
        assert(convert(7) == "Plong");

        // The sound for 6 is Pling as it has a factor 3
        assert(convert(6) == "Pling");

        // 2 to the power 3 does not make a raindrop sound as 3 is the exponent not the base
        assert(convert(8) == "8");

        // The sound for 9 is Pling as it has a factor 3
        assert(convert(9) == "Pling");

        // The sound for 10 is Plang as it has a factor 5
        assert(convert(10) == "Plang");

        // The sound for 14 is Plong as it has a factor of 7
        assert(convert(14) == "Plong");

        // The sound for 15 is PlingPlang as it has factors 3 and 5
        assert(convert(15) == "PlingPlang");

        // The sound for 21 is PlingPlong as it has factors 3 and 7
        assert(convert(21) == "PlingPlong");

        // The sound for 25 is Plang as it has a factor 5
        assert(convert(25) == "Plang");

        // The sound for 27 is Pling as it has a factor 3
        assert(convert(27) == "Pling");

        // The sound for 35 is PlangPlong as it has factors 5 and 7
        assert(convert(35) == "PlangPlong");

        // The sound for 49 is Plong as it has a factor 7
        assert(convert(49) == "Plong");

        // The sound for 52 is 52
        assert(convert(52) == "52");

        // The sound for 105 is PlingPlangPlong as it has factors 3, 5 and 7
        assert(convert(105) == "PlingPlangPlong");

        // The sound for 3125 is Plang as it has a factor 5
        assert(convert(3125) == "Plang");
    }

}
