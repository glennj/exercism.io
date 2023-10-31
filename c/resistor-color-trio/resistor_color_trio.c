#include "resistor_color_trio.h"
#include "my_math.h"
#include <stdio.h>

resistor_value_t color_code(resistor_band_t colors[]) {
    uint8_t two_bands = (10 * colors[0] + colors[1]);
    if (two_bands == 0)
        return (resistor_value_t){0, OHMS};

    resistor_value_t resistor = {.unit = OHMS};
    uint8_t power = colors[2];

    while (power >= 3) {
        resistor.unit++;
        power -= 3;
    }

    resistor.value = two_bands * int_pow(10, power);
    if (resistor.value % 1000 == 0) {
        resistor.unit++;
        resistor.value /= 1000;
    }

    return resistor;
}
