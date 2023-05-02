// experimenting with a functional approach

type CPCount record {|
    readonly int codepoint;
    int count;
|};

const int LOWER_A = 97;
const int LOWER_Z = 122;

function isLetter(int codepoint) returns boolean {
    return LOWER_A <= codepoint && codepoint <= LOWER_Z;
}

public function isIsogram(string sentence) returns boolean {
    var increment = function (table<CPCount> key(codepoint) t, int c) returns table<CPCount> {
        CPCount cpc = t.hasKey(c) ? t.get(c) : {codepoint: c, count: 0};
        cpc.count += 1;
        t.put(cpc);
        return t;
    };

    return sentence                     // string
        .toLowerAscii()                 // string
        .toCodePointInts()              // array of int
        .filter(isLetter)               // array of int
        .reduce(increment, table [])    // table of CPCount
        .filter(cpc => cpc.count > 1)   // table of CPCount
        .length() == 0;                 // boolean
}
