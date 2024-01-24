<?php

class ProgramWindow
{
    // tests demand these properties are untyped.
    public $x;
    public $y;
    public $width;
    public $height;

    function __construct() {
        $this->x = 0;
        $this->y = 0;
        $this->width = 800;
        $this->height = 600;
    }

    function resize(Size $size): void
    {
        $this->width = $size->width;
        $this->height = $size->height;
    }

    function move(Position $position): void
    {
        $this->x = $position->x;
        $this->y = $position->y;
    }
}
