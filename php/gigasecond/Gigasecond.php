<?php

declare(strict_types=1);

const GIGASECOND = new DateInterval('PT1000000000S');

function from(DateTimeImmutable $date): DateTimeImmutable
{
    return $date->add(GIGASECOND);
}
