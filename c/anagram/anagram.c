#include "anagram.h"
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <assert.h>

static int char_cmp(const void *a, const void *b) {
    char c1 = *(const char*)a;
    char c2 = *(const char*)b;
    if (c1 < c2) return -1;
    if (c1 > c2) return  1;
    return 0;
}

static char *make_key(const char *str, size_t length) {
    char *key = calloc(length + 1, sizeof *key);
    strcpy(key, str);
    qsort(key, length, sizeof *key, char_cmp);
    return key;
}

static char *strnlower(const char *str, size_t length) {
    char *lower = calloc(length + 1, sizeof *lower);
    char *lc = lower;
    for (size_t i = 0; i < length; i++)
        *lc++ = tolower(*str++);
    return lower;
}

// ------------------------------------------------------------
void find_anagrams(const char *subject, struct candidates *candidates) {
    assert(subject && candidates);

    size_t length = strlen(subject);
    char *subj_lower = strnlower(subject, length);
    char *subj_key = make_key(subj_lower, length);

    for (size_t i = 0; i < candidates->count; i++) {
        struct candidate *c = &candidates->candidate[i];
        if (strlen(c->word) != length) {
            c->is_anagram = NOT_ANAGRAM;
        }
        else {
            char *cand_lower = strnlower(c->word, length);
            char *cand_key = make_key(cand_lower, length);

            c->is_anagram = (
                    strncmp(subj_lower, cand_lower, length) != 0 &&
                    strncmp(subj_key, cand_key, length) == 0
            )   ? IS_ANAGRAM
                : NOT_ANAGRAM;

            free(cand_lower);
            free(cand_key);
        }
    }

    free(subj_lower);
    free(subj_key);
}
