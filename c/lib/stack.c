#include "stack.h"
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

stack_t *make_stack(unsigned size) {
    stack_t *stack = malloc(sizeof *stack);
    assert(stack != NULL);

    stack->buffer = malloc((size + 1) * sizeof stack->buffer);
    assert(stack->buffer != NULL);
    memset(stack->buffer, '\0', size + 1);

    stack->size = size;
    stack->idx = 0;
    return stack;
}

void destroy_stack(stack_t *stack) {
    assert(stack != NULL);
    free(stack->buffer);
    free(stack);
}

bool is_full(stack_t *stack) {
    assert(stack != NULL);
    return stack->idx == stack->size;
}

bool is_empty(stack_t *stack) {
    assert(stack != NULL);
    return stack->idx == 0;
}

bool push(stack_t *stack, char c) {
    assert(stack != NULL);
    if (is_full(stack))
        return false;
    stack->buffer[stack->idx++] = c;
    return true;
}

char pop(stack_t *stack) {
    assert(stack != NULL);
    if (is_empty(stack))
        return -1;
    char c = stack->buffer[--stack->idx];
    stack->buffer[stack->idx] = '\0';
    return c;
}
