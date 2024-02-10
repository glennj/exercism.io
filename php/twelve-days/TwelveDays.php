<?php

declare(strict_types=1);

const ORDINAL = [
    1 => 'first', 'second', 'third', 'fourth', 'fifth', 'sixth',
    'seventh', 'eighth', 'ninth', 'tenth', 'eleventh', 'twelfth',
];

const GIFTS = [
    1 => 'a Partridge in a Pear Tree.',
    'two Turtle Doves',
    'three French Hens',
    'four Calling Birds',
    'five Gold Rings',
    'six Geese-a-Laying',
    'seven Swans-a-Swimming',
    'eight Maids-a-Milking',
    'nine Ladies Dancing',
    'ten Lords-a-Leaping',
    'eleven Pipers Piping',
    'twelve Drummers Drumming',
];

class TwelveDays
{
    public function recite(int $start, int $end): string
    {
        return implode(PHP_EOL, array_map([$this, 'verse'], range($start, $end)));
    }

    private function verse(int $n): string
    {
        $gifts = array_slice(GIFTS, 0, $n);
        if ($n > 1) $gifts[0] = "and " . $gifts[0];
        return sprintf(
            'On the %s day of Christmas my true love gave to me: %s',
            ORDINAL[$n],
            implode(', ', array_reverse($gifts))
        );
    }
}
