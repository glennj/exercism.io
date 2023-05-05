public function distance(string strand1, string strand2) returns int|error {
    if strand1.length() != strand2.length() {
        return error("Unequal strand lengths");
    }

    return zip(strand1.toCodePointInts(), strand2.toCodePointInts())
           .filter(pair => pair[0] != pair[1])
           .length();
}

function zip(anydata[] a, anydata[] b) returns anydata[][] {
    return from int i in int:range(0, a.length(), 1) 
           select [a[i], b[i]];
}
