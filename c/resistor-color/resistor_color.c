#include "resistor_color.h"

uint16_t color_code(resistor_band_t color) {
    return (uint16_t)color;
}

resistor_band_t* colors() {
    /* -- boring, just hardcoding the enum as an array
    static resistor_band_t colors[] = {
        BLACK, BROWN, RED,    ORANGE, YELLOW,
        GREEN, BLUE,  VIOLET, GREY,   WHITE
    };
    */

    static resistor_band_t colors[WHITE - BLACK];
    for (uint16_t i = BLACK; i <= WHITE; i++) 
        colors[i] = (resistor_band_t)i;
        
    return colors;
}
