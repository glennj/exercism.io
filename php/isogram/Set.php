<?php

declare(strict_types=1);

class Set {
    private array $_data = [];

    public function add(mixed $item): void
    {
        $this->_data[$item] = true;
    }

    public function contains(mixed $item): bool
    {
        return array_key_exists($item, $this->_data);
    }

    public function to_array(): array
    {
        return array_keys($this->_data);
    }
}
