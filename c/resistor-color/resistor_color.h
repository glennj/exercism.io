#ifndef RESISTOR_COLOR_H
#define RESISTOR_COLOR_H

#include <stdint.h>
#include "resistor_color_colors.h"

const resistor_band_t* colors(void);

uint16_t color_code(resistor_band_t color);

#endif
