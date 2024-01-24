<?php

class Size
{
    public int $width;
    public int $height;

    function __construct(int $height, int $width) {
        $this->width = $width;
        $this->height = $height;
    }
}
