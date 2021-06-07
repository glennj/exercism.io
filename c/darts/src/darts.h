#ifndef DARTS_H
#define DARTS_H

#include <stdint.h>
#include <math.h>

#define DIST_BULLSEYE    1.0
#define DIST_MIDDLE      5.0
#define DIST_OUTER      10.0

#define SCORE_BULLSEYE  10
#define SCORE_MIDDLE     5
#define SCORE_OUTER      1
#define SCORE_NONE       0

struct coordinate {
    float x;
    float y;
};
typedef struct coordinate coordinate_t;

uint8_t score(coordinate_t coords);

#endif
