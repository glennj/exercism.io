<?php

class PizzaPi
{
    const MINIMUM_DOUGH_PER_PIZZA = 200;
    const EXTRA_DOUGH_PER_PERSON = 20;
    const SAUCE_PER_PIZZA = 125;
    const SAUCE_PER_CAN = 250;
    const SLICES_PER_PIZZA = 8;
    
    public function calculateDoughRequirement($num_pizzas, $num_people): int
    {
        return $num_pizzas * (
            self::MINIMUM_DOUGH_PER_PIZZA + self::EXTRA_DOUGH_PER_PERSON * $num_people
        );
    }

    public function calculateSauceRequirement($num_pizzas): int
    {
        return $num_pizzas * self::SAUCE_PER_PIZZA / self::SAUCE_PER_CAN;
    }

    public function calculateCheeseCubeCoverage(
        $cheese_cube_size, 
        $cheese_thickness, 
        $pizza_diameter
    ): int {
        $cube_volume = $cheese_cube_size ** 3;
        $pizza_cheese_volume = pi() * $cheese_thickness * $pizza_diameter;
        return ($cube_volume / $pizza_cheese_volume);
    }

    public function calculateLeftOverSlices($num_pizzas, $num_people)
    {
        return $num_pizzas * self::SLICES_PER_PIZZA % $num_people;
    }
}
