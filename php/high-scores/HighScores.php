<?php

declare(strict_types=1);

class HighScores
{
    public array $scores;
    public int $latest;
    public int $personalBest;
    public array $personalTopThree;

    public function __construct(array $scores)
    {
        $this->scores = $scores;
        $this->latest = end($scores);
        $this->personalBest = max(PHP_INT_MIN, ...$scores);
        rsort($scores);
        $this->personalTopThree = array_slice($scores, 0, 3);
    }
}
