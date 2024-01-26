<?php

declare(strict_types=1);

const BRACKETS = [ '}' => '{', ']' => '[', ')' => '(' ];

require_once 'Stack.php';

function brackets_match(string $input): bool
{
    $is_close_bracket = fn($char) => array_key_exists($char, BRACKETS);
    $is_open_bracket = fn($char) => in_array($char, array_values(BRACKETS));

    $stack = new Stack();
    foreach (str_split($input) as $char) {
        if ($is_open_bracket($char)) {
            $stack->push($char);
        } elseif ($is_close_bracket($char) && $stack->pop() != BRACKETS[$char]) {
            return false;
        }
    }

    return $stack->count == 0;
    // could also use:  return empty($stack);
}
