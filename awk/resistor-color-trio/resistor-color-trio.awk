#!/usr/bin/env gawk -f

@include "resistors"

{ 
    value = resistors::resistance($1, $2, $3)
    print resistors::with_units(value)
}
