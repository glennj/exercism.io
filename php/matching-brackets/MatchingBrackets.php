<?php

declare(strict_types=1);

const BRACKETS = [ '}' => '{', ']' => '[', ')' => '(' ];

function brackets_match(string $input): bool
{
    $is_close_bracket = fn($char) => array_key_exists($char, BRACKETS);
    $is_open_bracket = fn($char) => in_array($char, array_values(BRACKETS));

    $stack = [];
    foreach (str_split($input) as $char) {
        if ($is_open_bracket($char)) {
            array_push($stack, $char);
        } elseif ($is_close_bracket($char) && array_pop($stack) != BRACKETS[$char]) {
            return false;
        }
    }

    return count($stack) == 0;
    // could also use:  return empty($stack);
}
