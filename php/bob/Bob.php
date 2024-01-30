<?php

declare(strict_types=1);

class Bob
{
    public function respondTo(string $str): string
    {
        $str = rtrim($str);
        $isSilence = empty($str);
        $isQuestion = str_ends_with($str, '?');
        $isYelling = preg_match('/[[:upper:]]/', $str)
                 && !preg_match('/[[:lower:]]/', $str);

        if ($isSilence) {
            return 'Fine. Be that way!';
        } elseif ($isYelling && $isQuestion) {
            return "Calm down, I know what I'm doing!";
        } elseif ($isYelling) {
            return 'Whoa, chill out!';
        } elseif ($isQuestion) {
            return 'Sure.';
        } else {
            return 'Whatever.';
        }
    }
}
