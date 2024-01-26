<?php

declare(strict_types=1);

class Stack {
    private array $_data = [];
    private int $_count = 0;

    public function __get(string $property) {
        switch ($property) {
        case 'count':
            return $this->_count;
        default:
            trigger_error("Unknown property $property.");
        }
    }

    public function push(mixed $item): void
    {
        array_push($this->_data, $item);
        $this->_count++;
    }

    public function pop(): mixed
    {
        $this->_count--;
        return array_pop($this->_data);
    }

    public function peek(): mixed
    {
        return end($this->_data);
    }

    public function to_array(): array
    {
        return $this->_data;
    }
}
