#include "matching_brackets.h"
#include "stack.h"
#include <stdbool.h>
#include <string.h>
#include <assert.h>

bool is_paired(const char *input) {
    stack_t *stack = make_stack(strlen(input));

    for (const char *p = input; *p; p++) {
        // handle open brackets
        switch (*p) {
            case '(':
            case '[':
            case '{':
                assert(push(stack, *p));
                continue;
        }

        // handle close brackets and other chars
        char b;
        switch (*p) {
            case ')': b = '('; break;
            case ']': b = '['; break;
            case '}': b = '{'; break;
            default: continue;
        }

        if (b != pop(stack)) {
            destroy_stack(stack);
            return false;
        }
    }

    bool result = is_empty(stack);
    destroy_stack(stack);
    return result;
}

