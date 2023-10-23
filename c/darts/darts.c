#include "darts.h"

uint8_t score(coordinate_t coords) {
    float distance = hypot(coords.x, coords.y);

    if (distance <= RADIUS_BULLSEYE) return SCORE_BULLSEYE;
    if (distance <= RADIUS_MIDDLE)   return SCORE_MIDDLE;
    if (distance <= RADIUS_OUTER)    return SCORE_OUTER;
    return SCORE_NONE;
}
