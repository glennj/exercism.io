BEGIN {
    FS = ","
    dates["Monday"][0]    = 0; count["Monday"]    = 0
    dates["Tuesday"][0]   = 0; count["Tuesday"]   = 0
    dates["Wednesday"][0] = 0; count["Wednesday"] = 0
    dates["Thursday"][0]  = 0; count["Thursday"]  = 0
    dates["Friday"][0]    = 0; count["Friday"]    = 0
    dates["Saturday"][0]  = 0; count["Saturday"]  = 0
    dates["Sunday"][0]    = 0; count["Sunday"]    = 0
}

{
    date = 1
    t = mktime(sprintf("%d %d %d 12 34 56 0 1", $1, $2, date))
    while (0 + strftime("%-m", t, 1) == $2) {
        day = strftime("%A", t, 1)
        dates[day][++count[day]] = date
        date += 1
        t += 86400
    }
}

$3 == "first"  {day_of_month = dates[$4][1]}
$3 == "second" {day_of_month = dates[$4][2]}
$3 == "third"  {day_of_month = dates[$4][3]}
$3 == "fourth" {day_of_month = dates[$4][4]}
$3 == "last"   {day_of_month = dates[$4][count[$4]]}
$3 == "teenth" {day_of_month = dates[$4][2] >= 13 ? dates[$4][2] : dates[$4][3]}

{printf "%4d-%02d-%02d\n", $1, $2, day_of_month}
