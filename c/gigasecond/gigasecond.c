#include "gigasecond.h"

int const GIGASECOND = 1e9;

time_t gigasecond_after(time_t instant) {
    return instant + GIGASECOND;
}
