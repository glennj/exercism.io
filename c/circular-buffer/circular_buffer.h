#ifndef CIRCULAR_BUFFER_H
#define CIRCULAR_BUFFER_H
#include <stdlib.h>
#include <stdint.h>

typedef int buffer_value_t;
typedef int16_t status_t;

typedef struct {
    size_t read;
    size_t write;
} index_t;

typedef struct {
    buffer_value_t *data;
    size_t capacity;
    size_t count;
    index_t index;
} circular_buffer_t;

circular_buffer_t *new_circular_buffer(size_t capacity);
void delete_buffer(circular_buffer_t *buffer);
void clear_buffer(circular_buffer_t *buffer);

// These functions return EXIT_SUCCESS or EXIT_FAILURE.
// errno is set on failure.
status_t read(circular_buffer_t *buffer, buffer_value_t *value);
status_t write(circular_buffer_t *buffer, buffer_value_t value);
status_t overwrite(circular_buffer_t *buffer, buffer_value_t value);

#endif
