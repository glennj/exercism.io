<?php

declare(strict_types=1);
require_once 'extra_math_funcs.php';

class Clock
{
    private int $_minutes;

    public function __construct(int $hours, int $minutes = 0)
    {
        $this->_minutes = floormod($hours * 60 + $minutes, 24 * 60);
    }

    public function add(int $minutes): self
    {
        return new self(0, $this->_minutes + $minutes);
    }

    public function sub(int $minutes): self
    {
        return $this->add(-$minutes);
    }

    public function __toString(): string
    {
        return sprintf('%02d:%02d', ...(divmod($this->_minutes, 60)));
    }
}
