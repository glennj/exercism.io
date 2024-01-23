<?php

declare(strict_types=1);

class Lasagna
{
    const MINUTES_PER_LAYER = 2;

    function expectedCookTime()
    {
        return 40;
    }

    function remainingCookTime($elapsed_minutes)
    {
        return $this->expectedCookTime() - $elapsed_minutes;
    }

    function totalPreparationTime($layers_to_prep)
    {
        return $this::MINUTES_PER_LAYER * $layers_to_prep;
    }

    function totalElapsedTime($layers_to_prep, $elapsed_minutes)
    {
        return $this->totalPreparationTime($layers_to_prep) + $elapsed_minutes;
    }    

    function alarm()
    {
        return 'Ding!';  
    }
}
