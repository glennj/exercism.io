<?php

declare(strict_types=1);

function score(string $word): int
{
    if (strlen($word) == 0) return 0;
    return letter_score(substr($word, 0, 1)) + score(substr($word, 1));
}

function letter_score(string $letter): int
{
    switch(strtoupper($letter))
    {
        # not necessary to `break` since we are returning
        case "A": case "E": case "I": case "O": case "U":
        case "L": case "N": case "R": case "S": case "T":
            return 1;
        case "D": case "G":
            return 2;
        case "B": case "C": case "M": case "P":
            return 3;
        case "F": case "H": case "V": case "W": case "Y":
            return 4;
        case "K":
            return 5;
        case "J": case "X":
            return 8;
        case "Q": case "Z":
            return 10;
    }
}
