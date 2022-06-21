# Some math functions that awk doesn't include

@namespace "math"

function abs(a)    { return a < 0 ? -a : a }

function min(a, b) { return a < b ? a : b }
function max(a, b) { return a > b ? a : b }

function gcd(a, b) { return b == 0 ? a : gcd(b, a % b) }
