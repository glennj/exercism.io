<?php

declare(strict_types=1);

function transform(array $input): array
{
    return array_change_key_case(
        array_merge(...(
            array_map(
                fn ($score) => array_fill_keys($input[$score], (int) $score),
                array_keys($input)
            )
        ))
    );
}
