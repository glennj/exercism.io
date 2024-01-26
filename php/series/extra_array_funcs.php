<?php

/**
 * Some utility functions.
 * As if PHP didn't already have enough of them ...
 */

declare(strict_types=1);

/**
 * A generator function modelled after Ruby's `each_cons`
 *
 * - https://docs.ruby-lang.org/en/3.2/Enumerable.html#method-i-each_cons
 * - https://www.php.net/manual/en/language.generators.overview.php 
 *
 * @param $a The array over which to iterate.
 * @param $n The length of the slice to yield at each step.
 *
 * @return A Generator object.
 */
function array_each_cons(array $a, int $n): iterable
{
    $len = count($a);
    for ($i = 0; $i <= $len - $n; $i++) {
        yield array_slice($a, $i, $n);
    }
}
