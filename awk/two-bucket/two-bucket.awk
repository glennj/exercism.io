#!/usr/bin/env gawk -f

@include "assert"
@include "bucket"
@include "math"

BEGIN { FS = OFS = "," }

{
    assert($3 <= math::max($1, $2), "invalid goal") 
    gcd = math::gcd($1, $2)
    assert(gcd == 1 || $3 % gcd == 0, "invalid goal")

    b1 = bucket::new("one", $1)
    b2 = bucket::new("two", $2)
    switch ($4) {
        case "one": solve(b1, b2, $3); break
        case "two": solve(b2, b1, $3); break
    }
}

function solve(start, other, goal,    moves) {
    bucket::fill(start)
    moves = 1

    if (bucket::size(other) == goal) {
        bucket::fill(other)
        moves += 1
    }

    while (1) {
        if (bucket::amount(start) == goal) return result(moves, start, other)
        if (bucket::amount(other) == goal) return result(moves, other, start)

        if      (bucket::isEmpty(start)) bucket::fill(start)
        else if (bucket::isFull(other))  bucket::empty(other)
        else                             bucket::pour(start, other)
        moves += 1
    }
}

function result(moves, winner, loser) {
    print moves, bucket::name(winner), bucket::amount(loser)
}
