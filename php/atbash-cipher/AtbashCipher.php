<?php

declare(strict_types=1);

const PLAIN  = 'abcdefghijklmnopqrstuvwxyz';
const CIPHER = 'zyxwvutsrqponmlkjihgfedcba';

function encode(string $text): string
{
    $cleaned = strtolower(join(array_filter(str_split($text), 'ctype_alnum')));
    $encoded = strtr($cleaned, PLAIN, CIPHER);
    return join(' ', str_split($encoded, 5));
}
