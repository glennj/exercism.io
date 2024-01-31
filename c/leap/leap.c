#include "leap.h"

static bool approach_1(int year) {
    return year % 4 == 0 && (year % 100 != 0 || year % 400 == 0);
}

/*
static bool approach_2(int year) {
    if (year % 100 == 0) {
        return year % 400 == 0;
    } else {
        return year % 4 == 0;
    }
}
*/

/* approach 1 uses 1 to 3 comparisions per call;
 * approach 2 uses exactly 2 comparisons per call.
 *
 * Looking at the years 1 to 2000, how many total comparisons:
 *
 * divisible by 400 => 5
 * divisible by 100 => 15
 * divisible by   4 => 480
 * otherwise        => 1500
 *
 * approach 1: 5 * 3 + 15 * 3 + 480 * 2 + 1500 * 1 == 2520
 * approach 2: 2000 * 2 == 4000
 *
 * On the whole, approach 1 should be faster.
 */

bool leap_year(int year) {
    return approach_1(year);
}
