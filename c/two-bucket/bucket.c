#include "bucket.h"
#include "my_math.h"
#include <stdbool.h>
#include <stdlib.h>

bucket_t *make_bucket(bucket_id_t id, bucket_liters_t size) {
    bucket_t *b = malloc(sizeof *b);
    if (b) {
        b->id = id;
        b->size = size;
        b->amount = 0;
    }
    return b;
}

void destroy_bucket(bucket_t *b) {
    free(b);
}

bool is_full(bucket_t *b) {
    return b->amount == b->size;
}

bool is_empty(bucket_t *b) {
    return b->amount == 0;
}

void fill(bucket_t *b) {
    b->amount = b->size;
}

void empty(bucket_t *b) {
    b->amount = 0;
}

bucket_liters_t capacity(bucket_t *b) {
    return b->size - b->amount;
}

void pour(bucket_t *from, bucket_t *to) {
    bucket_liters_t quantity = umin(from->amount, capacity(to));
    from->amount -= quantity;
    to->amount += quantity;
}
