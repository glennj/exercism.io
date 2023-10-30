#include "space_age.h"

float age(planet_t planet, int64_t seconds) {
    if (planet < 0 || planet >= MAX_PLANET)
        return -1.0;

    return seconds / ORBITS[planet] / SECONDS_PER_EARTH_YEAR;
}
