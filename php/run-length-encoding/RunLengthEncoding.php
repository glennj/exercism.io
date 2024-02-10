<?php

declare(strict_types=1);

function encode(string $input): string
{
    return preg_replace_callback(
        '/(.)\1+/',
        fn ($m) => (string)strlen($m[0]) . $m[1],
        $input
    );
}

function decode(string $input): string
{
    return preg_replace_callback(
        '/(\d+)(.)/',
        fn ($m) => str_repeat($m[2], (int)$m[1]),
        $input
    );
}
