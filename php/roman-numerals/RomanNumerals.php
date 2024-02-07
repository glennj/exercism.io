<?php

declare(strict_types=1);

function toRoman(int $number, string $roman = ''): string
{
    $romanize = function ($base, $tenDigit, $fiveDigit, $oneDigit) use ($number, $roman) {
        if ($number >= 10 * $base) return toRoman($number - 10 * $base, $roman . $tenDigit);
        if ($number >=  9 * $base) return toRoman($number +      $base, $roman . $oneDigit);
        if ($number >=  5 * $base) return toRoman($number -  5 * $base, $roman . $fiveDigit);
        if ($number >=  4 * $base) return toRoman($number +      $base, $roman . $oneDigit);
        return                            toRoman($number -      $base, $roman . $oneDigit);
    };
    
    if ($number >= 400) return $romanize(100, 'M', 'D', 'C');
    if ($number >=  40) return $romanize(10,  'C', 'L', 'X');
    if ($number >=   1) return $romanize(1,   'X', 'V', 'I');
    return $roman;
}
