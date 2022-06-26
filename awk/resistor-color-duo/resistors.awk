@include "assert"

@namespace "resistors"

BEGIN {
    split("black brown red orange yellow green blue violet grey white", _Color_, / /)
    for (i in _Color_) Color[_Color_[i]] = i - 1
    delete _Color_

    Unit = "ohms"
    split("kilo mega giga", Prefix, / /)
}

function value(color) {
    awk::assert(color in Color, "invalid color '" color "'")
    return Color[color]
}

function resistance(a, b,    res) {
    res = 10 * value(a) + value(b)
    return res
}
