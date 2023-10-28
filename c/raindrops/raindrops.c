#include "raindrops.h"
#include <string.h>

// courtesy http://www.strudel.org.uk/itoa/
static char* itoa(int val, int base) {
	static char buf[32] = {0};
	int i = 30;
	for(; val && i ; --i, val /= base)
		buf[i] = "0123456789abcdef"[val % base];
	return &buf[i+1];
}

char *convert(char *result, int drops) {
    if (drops % 3 == 0) strcat(result, "Pling");
    if (drops % 5 == 0) strcat(result, "Plang");
    if (drops % 7 == 0) strcat(result, "Plong");

    if (*result == '\0') strcpy(result, itoa(drops, 10));

    return result;
}
