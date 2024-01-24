<?php

class HighSchoolSweetheart
{
    public function firstLetter(string $name): string
    {
        return substr(ltrim($name), 0, 1);
    }

    public function initial(string $name): string
    {
        return strtoupper($this->firstLetter($name)) . '.';
    }

    public function initials(string $name): string
    {
        //return implode(' ', array_map(fn($nm) => $this->initial($nm), explode(' ', $name)));
        return implode(' ', array_map([$this, 'initial'], explode(' ', $name)));
    }

    public function pair(string $sweetheart_a, string $sweetheart_b): string
    {
        $in_a = $this->initials($sweetheart_a);
        $in_b = $this->initials($sweetheart_b);
        return <<<HEART
                 ******       ******
               **      **   **      **
             **         ** **         **
            **            *            **
            **                         **
            **     $in_a  +  $in_b     **
             **                       **
               **                   **
                 **               **
                   **           **
                     **       **
                       **   **
                         ***
                          *
            HEART;
    }
}
