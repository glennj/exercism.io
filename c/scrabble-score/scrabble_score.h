#ifndef SCRABBLE_SCORE_H
#define SCRABBLE_SCORE_H
#include <stdint.h>

typedef uint8_t score_t;

score_t score(const char *word);

#endif
