#include "two_fer.h"
#include <stddef.h>

/*
#include <stdio.h>

void two_fer(char *buffer, const char *name) {
    sprintf(buffer, "One for %s, one for me.",
                    name == NULL ? "you" : name);
}
*/


#include <string.h>

void two_fer(char *buffer, const char *name) {
    strcpy(buffer, "One for ");
    strcat(buffer, name == NULL ? "you" : name);
    strcat(buffer, ", one for me.");
}
