@include "math"

@namespace "bucket"

BEGIN { Count = 0 }

function new(name, size,    id) {
    id = ++Count
    Name[id] = name
    Size[id] = size
    Amount[id] = 0
    return id
}

function name(id)    { return Name[id] }
function size(id)    { return Size[id] }
function amount(id)  { return Amount[id] }

function isFull(id)  { return Amount[id] == Size[id] }
function isEmpty(id) { return Amount[id] == 0 }

function fill(id)  { Amount[id] = Size[id] }
function empty(id) { Amount[id] = 0 }

function pour(from, to,    qty) {
    qty = math::min(Amount[from], Size[to] - Amount[to])
    Amount[from] -= qty
    Amount[to]   += qty
}

