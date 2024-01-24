<?php

/**
 * Some utility functions.
 * As if PHP didn't already have enough of them ...
 */

declare(strict_types=1);

/**
 * Get the index of an element in an array.
 *
 * @param $array  The array in which to search.
 * @param $wanted The element to be found.
 *
 * @return the key, or -1 if not found.
 */
function indexOf(array $array, mixed $wanted): int|string
{
    foreach ($array as $key => $element) {
        if ($element == $wanted) {
            return $key;
        }
    }
    return -1;
}

// nope, use `array_search` or `array_keys` instead.
