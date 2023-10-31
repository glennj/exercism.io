#include "clock.h"
#include <stdio.h>

const int MINUTES_PER_DAY = 24 * 60;

static int normalize(int min) {
    return ((min % MINUTES_PER_DAY) + MINUTES_PER_DAY) % MINUTES_PER_DAY;
}

static int minute_of_day(int hour, int minute) {
    return hour * 60 + minute;
}

static int parse_clock(clock_t clock) {
    int hour, minute;
    sscanf(clock.text, "%2d:%2d", &hour, &minute);
    return normalize(minute_of_day(hour, minute));
}

static clock_t make_clock(int mins) {
   mins = normalize(mins);
   clock_t clock;
   sprintf(clock.text, "%02d:%02d", mins / 60, mins % 60);
   return clock;
}

// ------------------------------------------------------------
clock_t clock_create(int hour, int minute) {
    return make_clock(minute_of_day(hour, minute));
}

clock_t clock_add(clock_t clock, int mins) {
    return make_clock(parse_clock(clock) + mins);
}

clock_t clock_subtract(clock_t clock, int mins) {
    return clock_add(clock, -mins);
}

bool clock_is_equal(clock_t a, clock_t b) {
    return parse_clock(a) == parse_clock(b);
}
