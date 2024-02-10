<?php

declare(strict_types=1);

class Series
{
    private $_digits;
    private $_len;

    public function __construct(string $input)
    {
        /* https://www.php.net/manual/en/intro.ctype.php
         * > It should be noted that ctype functions are always preferred over
         * > regular expressions, and even to some equivalent "str_*" and "is_*"
         * > functions. This is because of the fact that ctype uses a native C
         * > library and thus processes significantly faster.
         */
        if (!empty($input) && !ctype_digit($input)) {
            throw new InvalidArgumentException("non-digits");
        }

        $this->_digits = str_split($input);
        $this->_len = count($this->_digits);
    }

    public function largestProduct(int $span): int
    {
        if ($span < 1 || $span > $this->_len) {
            throw new InvalidArgumentException("invalid span");
        }

        return array_reduce(
            range(0, $this->_len - $span),
            fn($max, $offset) => max(
                $max,
                array_product(array_slice($this->_digits, $offset, $span))
            ),
            0
        );
    }
}
