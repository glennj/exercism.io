<?php

declare(strict_types=1);

const MAX = 99;

class BeerSong
{
    public function verse(int $n): string
    {
        $b = $this->_bottles($n);
        $l1 = sprintf('%s on the wall, %s.', $b, strtolower($b));

        $b = $this->_bottles($n - 1);
        $l2 = sprintf('%s, %s on the wall.', $this->_action($n), strtolower($b));

        return $l1 . "\n" . $l2 . ($n > 0 ? "\n" : '');
    }

    private function _bottles(int $n): string
    {
        if ($n < 0) {
            $n = MAX;
        }
        return sprintf(
            '%s bottle%s of beer',
            $n == 0 ? 'No more' : $n,
            $n == 1 ? '' : 's'
        );
    }

    private function _action(int $n): string
    {
        if ($n) {
            return 'Take ' . ($n == 1 ? 'it' : 'one') . ' down and pass it around';
        }
        return 'Go to the store and buy some more';
    }

    public function verses(int $start, int $finish): string
    {
        return join("\n", array_map([$this, 'verse'], range($start, $finish)));
    }

    public function lyrics(): string
    {
        return $this->verses(MAX, 0);
    }
}
