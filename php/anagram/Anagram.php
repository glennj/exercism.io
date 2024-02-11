<?php

declare(strict_types=1);

function detectAnagrams(string $word, array $anagrams): array
{
    $key = function ($word) {
        $chars = mb_str_split(mb_strtolower($word));
        sort($chars);
        return $chars;
    };

    $subj_lc = mb_strtolower($word);
    $subj_key = $key($word);

    // apparently the "assertEquals" test function, for arrays,
    // considers the keys having to be equal too. Thus, we need
    // to wrap the filter results to re-number the keys.
    return array_values(array_filter(
        $anagrams,
        fn($a) => $subj_lc != mb_strtolower($a) && $subj_key == $key($a)
    ));
}
