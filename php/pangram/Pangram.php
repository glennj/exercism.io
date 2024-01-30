<?php

declare(strict_types=1);

function isPangram(string $string): bool
{
    $a = ord('a');
    $mask = 0;

    foreach(str_split(strtolower($string)) as $char) {
        if ('a' <= $char && $char <= 'z') {
            $idx = ord($char) - $a;
            $mask |= 1 << $idx;
        }
    }

    return $mask == 0b0011_1111_1111_1111_1111_1111_1111;
}
