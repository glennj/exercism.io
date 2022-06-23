#!/usr/bin/env gawk -f

@include "resistors"

{ print resistors::resistance($1, $2) }
