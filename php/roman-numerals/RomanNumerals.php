<?php

declare(strict_types=1);

function toRoman(int $number, string $roman = ''): string
{
    if ($number >= 1000) return toRoman($number - 1000, $roman .  'M');
    if ($number >=  900) return toRoman($number -  900, $roman . 'CM');
    if ($number >=  500) return toRoman($number -  500, $roman .  'D');
    if ($number >=  400) return toRoman($number -  400, $roman . 'CD');
    if ($number >=  100) return toRoman($number -  100, $roman .  'C');
    if ($number >=   90) return toRoman($number -   90, $roman . 'XC');
    if ($number >=   50) return toRoman($number -   50, $roman .  'L');
    if ($number >=   40) return toRoman($number -   40, $roman . 'XL');
    if ($number >=   10) return toRoman($number -   10, $roman .  'X');
    if ($number >=    9) return toRoman($number -    9, $roman . 'IX');
    if ($number >=    5) return toRoman($number -    5, $roman .  'V');
    if ($number >=    4) return toRoman($number -    4, $roman . 'IV');
    if ($number >=    1) return toRoman($number -    1, $roman .  'I');
    return $roman;
}
