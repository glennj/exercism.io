<?php

declare(strict_types=1);

function accumulate(array $input, callable $accumulator): array
{
    $accumulated = [];
    foreach ($input as $element) {
        array_push($accumulated, $accumulator($element));
    }
    return $accumulated;
}
