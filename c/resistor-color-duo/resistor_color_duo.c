#include "resistor_color_duo.h"

uint16_t color_code(resistor_band_t *colors) {
    if (!colors) return 0;
    // what if the colors array has only 0 or 1 elements?
    return 10 * colors[0] + colors[1];
}
