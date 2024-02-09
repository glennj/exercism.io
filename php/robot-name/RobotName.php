<?php

declare(strict_types=1);

class Robot
{
    private static $used = [];
    private $_name;
    
    public function getName(): string
    {
        if (is_null($this->_name)) {
            $this->reset();
        }
        return $this->_name;
    }

    public function reset(): void
    {
        $letters = range('A', 'Z');

        // this approach will be quite slow if we need 676,000 robots
        do {
            $name = sprintf(
                '%s%s%03d',
                $letters[array_rand($letters)],
                $letters[array_rand($letters)],
                rand(0, 999)
            );
        } while (array_key_exists($name, self::$used));

        self::$used[$name] = true;
        $this->_name = $name;
    }
}
