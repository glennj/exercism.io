#include "resistor_color_duo.h"
#include <stddef.h>

uint16_t color_code(resistor_band_t *colors) {
    if (colors == NULL) return 0;
    return 10 * colors[0] + colors[1];
}

/* A potential bug here:
 * if the user passes in a colors array that has only 1 element,
 * then the value of `colors[1]` is not part of the array and could be anything.
 * This function, as declared, is assuming good faith by the user.
 * A length argument should be included for safety.
 *
 * Oh well, garbage in garbage out.
 */
