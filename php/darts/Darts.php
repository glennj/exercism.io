<?php

// An approach using a dynamic getter instead of a real property.

declare(strict_types=1);

class Darts
{
    private float $_distance;

    public function __construct(float $x, float $y)
    {
        $this->_distance = hypot($x, $y);
    }

    public function __get($property)
    {
        if ($property != 'score') {
            trigger_error("Unknown property $property");
        }
        /*
        if ($this->_distance <= 1.0) {
            return 10;
        } elseif ($this->_distance <= 5.0) {
            return 5;
        } elseif ($this->_distance <= 10.0) {
            return 1;
        } else {
            return 0;
        }
        */
        // I can't decide if this is prettier...
        return ($this->_distance <=  1.0) ? 10 : (
               ($this->_distance <=  5.0) ?  5 : (
               ($this->_distance <= 10.0) ?  1 : 0));
    }
}
