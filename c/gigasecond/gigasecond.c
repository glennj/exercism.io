#include "gigasecond.h"

#define GIGASECOND 1e9

void gigasecond(time_t before, char *result, size_t size) {
    time_t after = before + GIGASECOND;
    strftime(result, size, "%Y-%m-%d %H:%M:%S", gmtime(&after));
}
