#include "scrabble_score.h"
#include <ctype.h>

#define TAKE 3

#if TAKE == 1

static score_t letter_score(const char letter) {
    switch (letter) {
        case 'A': case 'E': case 'I': case 'O': case 'U':
        case 'L': case 'N': case 'R': case 'S': case 'T':
            return 1;
        case 'D': case 'G':
            return 2;
        case 'B': case 'C': case 'M': case 'P':
            return 3;
        case 'F': case 'H': case 'V': case 'W': case 'Y':
            return 4;
        case 'K':
            return 5;
        case 'J': case 'X':
            return 8;
        case 'Q': case 'Z':
            return 10;
        default:
            return 0;
    }
}

#elif TAKE == 2
// with inspiration from
// https://exercism.org/tracks/c/exercises/raindrops/solutions/bobahop

typedef struct {
    char letter;
    score_t value;
} tile_t;

// arranged by letter frequency
// https://web.archive.org/web/20111224230632/http://oxforddictionaries.com/words/what-is-the-frequency-of-the-letters-of-the-alphabet-in-english
static const tile_t tiles[] = {
    {'E', 1}, {'A', 1}, {'R', 1}, {'I', 1}, {'O', 1}, {'T', 1},
    {'N', 1}, {'S', 1}, {'L', 1}, {'C', 3}, {'U', 1}, {'D', 2},
    {'P', 3}, {'M', 3}, {'H', 4}, {'G', 2}, {'B', 3}, {'F', 4},
    {'Y', 4}, {'W', 4}, {'K', 5}, {'V', 4}, {'X', 8}, {'Z', 10},
    {'J', 8}, {'Q', 10},
};

static const int TILE_COUNT = sizeof(tiles) / sizeof(tile_t);

static score_t letter_score(const char letter) {
    for (int i = 0; i < TILE_COUNT; i++)
        if (tiles[i].letter == letter)
            return tiles[i].value;
    return 0;
}

#elif TAKE == 3
// from the Dig Deeper page, direct lookup without looping

static const score_t values[26] = {1,3,3,2,1,4,2,4,1,8,5,1,3,1,1,3,10,1,1,1,1,4,4,8,4,10};
//                                 A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,_Q,R,S,T,U,V,W,X,Y,_Z

static score_t letter_score(const char letter) {
    return values[letter - 'A'];
}

#endif

score_t score(const char *word) {
    if (!word)
        return 0;

    score_t s = 0;
    for (; *word; word++) {
        char letter = toupper(*word);
        if ('A' <= letter && letter <= 'Z')
            s += letter_score(letter);
    }
    return s;
}
