<?php

declare(strict_types=1);

const A = 97; // ord('a');
const ALPHABET_SIZE = 26;
const KEY_SIZE = 100;
const KEY_PATTERN = '/^[a-z]+$/';

class SimpleCipher
{
    public $key;

    public function __construct(string $key = null)
    {
        if (is_null($key)) {
            $this->key = $this->_randomKey();
        } elseif (preg_match(KEY_PATTERN, $key)) {
            $this->key = $key;
        } else {
            throw new InvalidArgumentException();
        }
    }

    public function encode(string $plainText): string
    {
        return $this->_encipher($plainText, +1);
    }

    public function decode(string $cipherText): string
    {
        return $this->_encipher($cipherText, -1);
    }

    private function _encipher(string $text, int $direction): string
    {
        $len = strlen($text);
        while (strlen($this->key) < $len) {
            $this->key .= $this->key;
        }
        for (
            $encoded = '', $i = 0;
            $i < $len;
            $encoded .= $this->_cipherChar($text[$i], $this->key[$i], $direction), $i++
        );
        return $encoded;
    }

    private function _cipherChar($char, $key, $direction): string
    {
        $c = ord($char) - A;
        $k = ord($key) - A;
        $cipher = ($c + $direction * $k + ALPHABET_SIZE) % ALPHABET_SIZE;
        return chr($cipher + A);
    }

    private function _randomKey(): string
    {
        $letters = range('a', 'z');
        for (
            $key = '', $i = 0;
            $i < KEY_SIZE;
            $key .= $letters[array_rand($letters)], $i++
        );
        return $key;
    }
}
