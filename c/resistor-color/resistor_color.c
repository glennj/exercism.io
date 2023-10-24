#include "resistor_color.h"

#define BORING true

uint16_t color_code(resistor_band_t color) {
    return color;
}

const resistor_band_t* colors(void) {
#if BORING
    /* Just hardcoding the enum as an array.
     * It doesn't have to figure out the array size though.
     */
    static resistor_band_t colors[] = {
        BLACK, BROWN, RED, ORANGE, YELLOW,
        GREEN, BLUE, VIOLET, GREY, WHITE
    };
#else
    /* This version still needs _some_ inside knowledge about
     * the color names and values:
     *      BLACK is the smallest, WHITE is the largest
     */
    static resistor_band_t colors[WHITE - BLACK + 1];
    for (uint16_t i = BLACK; i <= WHITE; i++)
        colors[i] = (resistor_band_t)i;
#endif

    return colors;
}

/* Notes
 * - `static` to state that the array has "storage duration" for
 *   the lifetime of the program execution, *and* the value is
 *   initialized once
 *      https://en.cppreference.com/w/c/language/storage_duration#Storage_duration
 * - `const` to state that the returned pointer is read-only:
 *      https://en.cppreference.com/w/c/language/type#Compatible_types
 *   this will not compile:
 *          const resistor_band_t* cs = colors();
 *          c[1] = WHITE;
 *
 *   Compiling tests.out
 *   ./resistor_color.c: In function ‘color_code’:
 *   ./resistor_color.c:7:11: error: assignment of read-only location ‘*(cs + 4)’
 *       7 |     cs[1] = WHITE;
 *         |           ^
 *   make: *** [makefile:37: tests.out] Error 1
 */
