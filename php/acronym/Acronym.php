<?php

declare(strict_types=1);

function acronym(string $text): string
{
    $state = 'seeking_alpha';
    $acronym = '';
    $prev = '';
    $is_letter = fn($char) => mb_ereg_match('\p{L}', $char);
    $is_upper = fn($char) => $char == mb_strtoupper($char);
    $is_lower = fn($char) => $char == mb_strtolower($char);

    foreach (mb_str_split($text) as $char) {
        if ($state == 'seeking_alpha') {
            if ($is_letter($char)) {
                $acronym .= $char;
                $state = 'seeking_non_alpha';
            }
        } elseif (!$is_letter($char)) {
            $state = 'seeking_alpha';
        } elseif ($is_lower($prev) && $is_upper($char)) {
            $acronym .= $char;
        }
        $prev = $char;
    }

    return strlen($acronym) < 2 ? "" : mb_strtoupper($acronym);
}

