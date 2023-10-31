#ifndef RESISTOR_COLOR_TRIO_H
#define RESISTOR_COLOR_TRIO_H

#include <stdint.h>
#include "resistor_color_colors.h"

typedef enum {
    OHMS = 0,
    KILOOHMS,
    MEGAOHMS,
    GIGAOHMS
} resistor_unit_t;

typedef struct {
    uint16_t value;
    resistor_unit_t unit;
} resistor_value_t;

resistor_value_t color_code(resistor_band_t colors[]);

#endif
