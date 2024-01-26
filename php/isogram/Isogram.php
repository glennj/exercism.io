<?php

declare(strict_types=1);

require_once 'Set.php';

function isIsogram(string $word): bool
{
    // `mb_ereg_*` functions do _not_ use fences around the regex: `/\P{L}/`.
    // $cleaned = mb_ereg_replace('\P{L}', '', $word);

    // `preg_*` functions need the `u` flag on the pattern to treat
    // _both_ the pattern and the subject as UTF-8 data.
    $cleaned = preg_replace('/\P{L}/u', '', $word);

    $seen = new Set();
    foreach (mb_str_split(mb_strtolower($cleaned)) as $char) {
        if ($seen->contains($char)) {
            return false;
        }
        $seen->add($char);
    }
    return true;
}
