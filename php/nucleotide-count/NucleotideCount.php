<?php

declare(strict_types=1);

function nucleotideCount(string $input): array
{
    return array_reduce(
        str_split(strtolower($input)),
        function ($count, $char) {
            $count[$char] += 1;
            return $count;
        },
        ['a' => 0, 'c' => 0, 't' => 0, 'g' => 0]
    );
}
