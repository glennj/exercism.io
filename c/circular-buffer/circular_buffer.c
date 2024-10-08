#include "circular_buffer.h"
#include <stdbool.h>
#include <assert.h>
#include <errno.h>

#define DEBUG 0

#if DEBUG
#include <stdio.h>

static void print_buffer(circular_buffer_t *b, char *prefix) {
    fprintf(stderr, "%s: %p (%ld/%ld) [%ld,%ld] - {",
                prefix,
                (void *)b,
                b->count,
                b->capacity,
                b->index.read,
                b->index.write
    );
    for (size_t i = 0; i < b->capacity; i++)
        fprintf(stderr, "%d,", b->data[i]);
    fprintf(stderr, "\b}\n");
}
#endif

static bool is_empty(circular_buffer_t *b) {
    return b->count == 0;
}

static bool is_full(circular_buffer_t *b) {
    return b->count == b->capacity;
}

static void incr_idx(circular_buffer_t *b, size_t *idx) {
    *idx = (*idx + 1) % b->capacity;
}

// ------------------------------------------------------------
circular_buffer_t *new_circular_buffer(size_t capacity) {
    circular_buffer_t *b = malloc(sizeof *b);
    if (b == NULL) 
        return NULL;
    b->data = calloc(capacity, sizeof b->data);
    if (b->data == NULL) {
        free(b);
        return NULL;
    }
    b->capacity = capacity;
    clear_buffer(b);
#if DEBUG
    print_buffer(b, "init");
#endif
    return b;
}

// ------------------------------------------------------------
void clear_buffer(circular_buffer_t *b) {
    if (b == NULL) 
        return;
    b->count = 0;
    b->index = (index_t){0, 0};
}

// ------------------------------------------------------------
void delete_buffer(circular_buffer_t *b) {
    if (b == NULL) 
        return;
    free(b->data);
    free(b);
}

// ------------------------------------------------------------
status_t write(circular_buffer_t *b, buffer_value_t value) {
    if (b == NULL) {
        errno = EINVAL;
        return EXIT_FAILURE;
    }
#if DEBUG
    print_buffer(b, "write start");
#endif
    if (is_full(b)) {
        errno = ENOBUFS;
        return EXIT_FAILURE;
    }

    b->data[b->index.write] = value;
    incr_idx(b, &b->index.write);
    b->count++;
#if DEBUG
    print_buffer(b, "write end");
#endif
    return EXIT_SUCCESS;
}

// ------------------------------------------------------------
status_t read(circular_buffer_t *b, buffer_value_t *value) {
    if (b == NULL) {
        errno = EINVAL;
        return EXIT_FAILURE;
    }
#if DEBUG
    print_buffer(b, "read start");
#endif
    if (is_empty(b)) {
        errno = ENODATA;
        return EXIT_FAILURE;
    }

    *value = b->data[b->index.read];
    incr_idx(b, &b->index.read);
    b->count--;
#if DEBUG
    print_buffer(b, "read end");
#endif
    return EXIT_SUCCESS;
}

// ------------------------------------------------------------
status_t overwrite(circular_buffer_t *b, buffer_value_t value) {
    if (b == NULL) {
        errno = EINVAL;
        return EXIT_FAILURE;
    }
    if (is_full(b)) {
        buffer_value_t ignore;
        read(b, &ignore);
    }
    return write(b, value);
}
