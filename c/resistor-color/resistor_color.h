#ifndef RESISTOR_COLOR_H
#define RESISTOR_COLOR_H

#include <stdint.h>

typedef enum {
    BLACK, BROWN, RED,    ORANGE, YELLOW,
    GREEN, BLUE,  VIOLET, GREY,   WHITE
} resistor_band_t;

resistor_band_t* colors();

uint16_t color_code(resistor_band_t color);

#endif

