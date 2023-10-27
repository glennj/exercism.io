#include "bob.h"
#include <ctype.h>
#include <stdbool.h>

char *hey_bob(char *greeting) {
    bool has_upper = false;
    bool has_lower = false;
    char last_nonwhitespace = '\0';

    for (char *p = greeting; *p; p++) {
        if (isupper(*p)) has_upper = true;
        if (islower(*p)) has_lower = true;
        if (!isspace(*p)) last_nonwhitespace = *p;
    }

    bool is_yelling = has_upper && !has_lower;
    bool is_asking = last_nonwhitespace == '?';
    bool is_silent = last_nonwhitespace == '\0';

    if (is_yelling && is_asking) return "Calm down, I know what I'm doing!";
    if (is_yelling) return "Whoa, chill out!";
    if (is_asking) return "Sure.";
    if (is_silent) return "Fine. Be that way!";
    return "Whatever.";
}
