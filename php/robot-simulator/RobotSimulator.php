<?php

declare(strict_types=1);

class Robot
{
    const DIRECTION_EAST  = 'east';
    const DIRECTION_NORTH = 'north';
    const DIRECTION_WEST  = 'west';
    const DIRECTION_SOUTH = 'south';

    const LEFT_TURNS = [
        self::DIRECTION_EAST => self::DIRECTION_NORTH, 
        self::DIRECTION_NORTH => self::DIRECTION_WEST, 
        self::DIRECTION_WEST => self::DIRECTION_SOUTH,
        self::DIRECTION_SOUTH => self::DIRECTION_EAST
    ];

    const ADVANCEMENTS = [
        self::DIRECTION_EAST  => [1, 0],
        self::DIRECTION_NORTH => [0, 1],
        self::DIRECTION_WEST  => [-1, 0],
        self::DIRECTION_SOUTH => [0, -1],
    ];

    private array $_position;
    private string $_direction;

    public function __construct(array $position, string $direction)
    {
        $this->_position = $position;
        $this->_direction = $direction;
    }

    public function __get(string $property)
    {
        switch ($property) {
        case 'position':
            return $this->_position;
        case 'direction':
            return $this->_direction;
        default:
            trigger_error("Unknown property: $property");
        }
    }

    public function turnLeft(): self
    {
        $this->_direction = self::LEFT_TURNS[$this->_direction];
        return $this;
    }

    public function turnRight(): self
    {
        $this->_direction = (array_flip(self::LEFT_TURNS))[$this->_direction];
        return $this;
    }

    public function advance(): self
    {
        [$dx, $dy] = self::ADVANCEMENTS[$this->_direction];
        $this->_position[0] += $dx;
        $this->_position[1] += $dy;
        return $this;
    }

    public function instructions(string $instructions): self
    {
        $dispatch = [
            'R' => 'turnRight',
            'L' => 'turnLeft',
            'A' => 'advance'
        ];

        foreach (str_split($instructions) as $instruction) {
            if (!array_key_exists($instruction, $dispatch)) {
                throw new InvalidArgumentException('Invalid instruction');
            }
            $method = $dispatch[$instruction];
            $this->$method();
        }
        return $this;
    }
}
