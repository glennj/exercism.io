#ifndef SPACE_AGE_H
#define SPACE_AGE_H

#include <stdint.h>

typedef enum planet {
   MERCURY = 0,
   VENUS,
   EARTH,
   MARS,
   JUPITER,
   SATURN,
   URANUS,
   NEPTUNE,
   MAX_PLANET
} planet_t;

static const float ORBITS[] = {
    0.2408467f,
    0.61519726f,
    1.0f,
    1.8808158f,
    11.862615f,
    29.447498f,
    84.016846f,
    164.79132f,
};

static const int SECONDS_PER_EARTH_YEAR = 31557600;

float age(planet_t planet, int64_t seconds);

#endif
