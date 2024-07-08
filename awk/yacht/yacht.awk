BEGIN { FS = "," }

$NF == "ones"   {print sum_of(1)}
$NF == "twos"   {print sum_of(2)}
$NF == "threes" {print sum_of(3)}
$NF == "fours"  {print sum_of(4)}
$NF == "fives"  {print sum_of(5)}
$NF == "sixes"  {print sum_of(6)}

$NF == "full house"      {print full_house()}
$NF == "four of a kind"  {print four_of_a_kind()}
$NF == "little straight" {print straight(0)}
$NF == "big straight"    {print straight(1)}

$NF == "yacht" {print ($1 == $2 && $2 == $3 && $3 == $4 && $4 == $5) ? 50 : 0}
$NF == "choice" {print sum()}

# -------------------------------------------------
function sum_of(die,   i, sum) {
    sum = 0
    for (i = 1; i <= 5; i++)
        if ($i == die)
            sum += die
    return sum
}

function sorted(    a, i) {
    for (i = 1; i <= 5; i++) a[i] = $i
    asort(a)
    for (i = 1; i <= 5; i++) $i = a[i]
}

function full_house() {
    sorted()
    if ($1 == $2 && $4 == $5) {
        return (($2 == $3 || $3 == $4) && $1 != $5) ? sum() : 0
    }
    return 0
}

function four_of_a_kind() {
    sorted()
    return ($2 == $3 && $3 == $4 && ($1 == $2 || $4 == $5)) ? 4 * $2 : 0
}

function straight(offset,    a, i) {
    sorted()
    for (i = 1; i <= 5; i++)
        if ($i != offset + i)
            return 0
    return 30
}

function sum() {
    return $1 + $2 + $3 + $4 + $5
}
