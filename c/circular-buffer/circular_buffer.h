#ifndef CIRCULAR_BUFFER_H
#define CIRCULAR_BUFFER_H
#include <stdlib.h>
#include <stdint.h>

typedef int buffer_value_t;

typedef struct {
    size_t read;
    size_t write;
} head_t;

typedef struct {
    buffer_value_t *data;
    size_t capacity;
    head_t head;
    size_t count;
} circular_buffer_t;

circular_buffer_t *new_circular_buffer(size_t capacity);
void delete_buffer(circular_buffer_t *buffer);
void clear_buffer(circular_buffer_t *buffer);
int16_t read(circular_buffer_t *buffer, buffer_value_t *value);
int16_t write(circular_buffer_t *buffer, buffer_value_t value);
int16_t overwrite(circular_buffer_t *buffer, buffer_value_t value);

#endif
