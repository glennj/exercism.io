<?php

declare(strict_types=1);

function parse_binary(string $binary): int
{
    return array_reduce(
        str_split($binary),
        function ($decimal, $digit) {
            switch ($digit) {
            case '0':
            case '1':
                return $decimal << 1 | $digit;
            default:
                throw new InvalidArgumentException('not a binary digit');
            };
        },
        0
    );
}
