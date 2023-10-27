#ifndef STACK_H
#define STACK_H

#include <stdbool.h>

typedef struct {
    char *buffer;
    unsigned size;
    unsigned idx;
} stack_t;

stack_t *make_stack(unsigned size);

void destroy_stack(stack_t *stack);

// push will return false if the stack is full
bool push(stack_t *stack, char c);

// pop will return -1 if the stack is empty
char pop(stack_t *stack);

bool is_full(stack_t *stack);
bool is_empty(stack_t *stack);

#endif
