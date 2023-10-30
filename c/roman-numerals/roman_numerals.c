#include "roman_numerals.h"
#include <stdlib.h>
#include <assert.h>

#define MAX_ROMAN 3999
#define BUFF_SIZE 15

char *to_roman_numeral(unsigned n) {
    assert(n <= MAX_ROMAN);   

    char *roman = calloc(BUFF_SIZE + 1, sizeof *roman);
    assert(roman != NULL);
    char *p = roman;

    while (n >= 1000) {n -= 1000; *p++ = 'M';}
    if    (n >=  900) {n -=  900; *p++ = 'C'; *p++ = 'M';}
    if    (n >=  500) {n -=  500; *p++ = 'D';}
    if    (n >=  400) {n -=  400; *p++ = 'C'; *p++ = 'D';}
    while (n >=  100) {n -=  100; *p++ = 'C';}
    if    (n >=   90) {n -=   90; *p++ = 'X'; *p++ = 'C';}
    if    (n >=   50) {n -=   50; *p++ = 'L';}
    if    (n >=   40) {n -=   40; *p++ = 'X'; *p++ = 'L';}
    while (n >=   10) {n -=   10; *p++ = 'X';}
    if    (n >=    9) {n -=    9; *p++ = 'I'; *p++ = 'X';}
    if    (n >=    5) {n -=    5; *p++ = 'V';}
    if    (n >=    4) {n -=    4; *p++ = 'I'; *p++ = 'V';}
    while (n >=    1) {n -=    1; *p++ = 'I';}

    return roman;
}
