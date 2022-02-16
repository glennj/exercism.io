#include "darts.h"

uint8_t score(coordinate_t coords) {
    float dist = hypot(coords.x, coords.y);

    if (dist <= DIST_BULLSEYE) return SCORE_BULLSEYE;
    if (dist <= DIST_MIDDLE)   return SCORE_MIDDLE;
    if (dist <= DIST_OUTER)    return SCORE_OUTER;
    return SCORE_NONE;
}