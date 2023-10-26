#ifndef TWO_BUCKET_H
#define TWO_BUCKET_H

#include "bucket.h"
#include <stdbool.h>

typedef struct {
    bool possible;
    int move_count;
    bucket_id_t goal_bucket;
    bucket_liters_t other_bucket_liters;
} bucket_result_t;

bucket_result_t measure(
        bucket_liters_t bucket_1_size,
        bucket_liters_t bucket_2_size,
        bucket_liters_t goal_volume,
        bucket_id_t start_bucket
);

#endif
