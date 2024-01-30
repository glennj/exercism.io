<?php

declare(strict_types=1);

class SpaceAge
{
    private const RELATIVE_ORBIT = [
        'Mercury' =>   0.2408467,
        'Venus'   =>   0.61519726,
        'Earth'   =>   1.0,
        'Mars'    =>   1.8808158,
        'Jupiter' =>  11.862615,
        'Saturn'  =>  29.447498,
        'Uranus'  =>  84.016846,
        'Neptune' => 164.79132,
    ];
    private const SECONDS_PER_EARTH_YEAR = 31557600;
    private int $_seconds;

    public function __construct(int $seconds)
    {
        $this->_seconds = $seconds;
    }

    public function seconds(): int
    {
        return $this->_seconds;
    }

    public function __call(string $planet, array $args): float
    {
        $Planet = ucfirst($planet);
        if (!array_key_exists($Planet, self::RELATIVE_ORBIT)) {
            trigger_error("Error: $Planet is not a planet");
        }
        return $this->_seconds
                / self::RELATIVE_ORBIT[$Planet]
                / self::SECONDS_PER_EARTH_YEAR;
    }
}
