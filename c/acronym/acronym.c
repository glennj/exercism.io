#include "acronym.h"
#include <stdlib.h>
#include <stdbool.h>
#include <ctype.h>
#include <assert.h>

#define ABBR_SIZE 64

typedef enum {SEEKING_LETTER, SEEKING_NON_LETTER} State;

static bool is_abbr_letter_char(char c) {
    return isalpha(c) || c == '\'';
}

char *abbreviate(const char *phrase) {
    if (!(phrase && *phrase))   // null input or empty input
        return NULL;

    char *abbr = calloc(ABBR_SIZE, sizeof *abbr);
    assert(abbr);

    char *p = abbr;
    State state = SEEKING_LETTER;
    for (; *phrase; phrase++)
        switch (state) {
            case SEEKING_LETTER:
                if (isalpha(*phrase)) {
                    *p++ = toupper(*phrase);
                    state = SEEKING_NON_LETTER;
                }
                break;
            case SEEKING_NON_LETTER:
                if (!is_abbr_letter_char(*phrase))
                    state = SEEKING_LETTER;
                break;
        }

    return abbr;
}
