BEGIN { minsPerDay = 24 * 60 }

function normalize(mins)       { return (mins % minsPerDay + minsPerDay) % minsPerDay }
function create(hrs, mins)     { return normalize(60 * hrs + mins) }
function add(hrs, mins, delta) { return normalize(create(hrs, mins) + delta) }
function printClock(mins)      { printf "%02d:%02d\n", mins / 60, mins % 60 }

$1 == "create"   { printClock(create($2, $3)) }
$1 == "add"      { printClock(add($2, $3, $4)) }
$1 == "subtract" { printClock(add($2, $3, -$4)) }
$1 == "equal"    { print create($2, $3) == create($4, $5) ? "true" : "false" }
