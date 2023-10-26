#include "two_bucket.h"
#include "my_math.h"
#include <assert.h>
#include <stdbool.h>
#include <stddef.h>

static bucket_result_t const RESULT_INVALID = { .possible = false };

static bucket_result_t result(
        bucket_t *winner,
        bucket_t *loser,
        int moves
) {
    return (bucket_result_t){
        .possible = true,
        .move_count = moves,
        .goal_bucket = winner->id,
        .other_bucket_liters = loser->amount
    };
}

static bucket_result_t solve(
        bucket_t *first,
        bucket_t *second,
        bucket_liters_t goal
) {
    int moves = 0;

    fill(first);
    moves++;

    if (second->size == goal) {
        fill(second);
        moves++;
    }

    while (true) {
        if (first->amount == goal)
            return result(first, second, moves);
        if (second->amount == goal)
            return result(second, first, moves);

        if (is_empty(first))
            fill(first);
        else if (is_full(second))
            empty(second);
        else
            pour(first, second);

        moves++;
    }
}

static bool valid_parameters(
        bucket_liters_t b1,
        bucket_liters_t b2,
        bucket_liters_t goal
) {
    unsigned g = gcd(b1, b2);
    return (goal <= umax(b1, b2) && (g == 1 || goal % g == 0));
}

// ------------------------------------------------------------

bucket_result_t measure(
        bucket_liters_t bucket_1_size,
        bucket_liters_t bucket_2_size,
        bucket_liters_t goal_volume,
        bucket_id_t start_bucket
) {
    if (!valid_parameters(bucket_1_size, bucket_2_size, goal_volume))
        return RESULT_INVALID;

    bucket_t *one = make_bucket(BUCKET_ID_1, bucket_1_size);
    bucket_t *two = make_bucket(BUCKET_ID_2, bucket_2_size);
    assert(one != NULL && two != NULL);

    bucket_result_t result;
    if (start_bucket == BUCKET_ID_1)
        result = solve(one, two, goal_volume);
    else
        result = solve(two, one, goal_volume);

    destroy_bucket(one);
    destroy_bucket(two);
    return result;
}
