#ifndef DARTS_H
#define DARTS_H

#include <stdint.h>

struct coordinate {
    float x;
    float y;
};
typedef struct coordinate coordinate_t;

uint8_t score(coordinate_t coords);

#endif
