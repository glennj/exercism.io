#include "darts.h"
#include <math.h>

#define SCORE_BULLSEYE  10
#define SCORE_MIDDLE     5
#define SCORE_OUTER      1
#define SCORE_NONE       0

#define RADIUS_BULLSEYE  1.0
#define RADIUS_MIDDLE    5.0
#define RADIUS_OUTER    10.0

uint8_t score(coordinate_t coords) {
    float distance = hypot(coords.x, coords.y);

    if (distance <= RADIUS_BULLSEYE) return SCORE_BULLSEYE;
    if (distance <= RADIUS_MIDDLE)   return SCORE_MIDDLE;
    if (distance <= RADIUS_OUTER)    return SCORE_OUTER;
    return SCORE_NONE;
}
