#ifndef BUCKET_H
#define BUCKET_H

#include <stdbool.h>

typedef enum { BUCKET_ID_1, BUCKET_ID_2 } bucket_id_t;

typedef unsigned int bucket_liters_t;

typedef struct {
    bucket_id_t     id;
    bucket_liters_t size;
    bucket_liters_t amount;
} bucket_t;

bucket_t *make_bucket(bucket_id_t id, bucket_liters_t size);
void destroy_bucket(bucket_t *bucket);

bool is_full(bucket_t *bucket);
bool is_empty(bucket_t *bucket);
void fill(bucket_t *bucket);
void empty(bucket_t *bucket);
bucket_liters_t capacity(bucket_t *bucket);
void pour(bucket_t *from_bucket, bucket_t *to_bucket);

#endif
