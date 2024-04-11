namespace scrabble_score

sequence tiles = {
    1, -- A
    3, -- B
    3, -- C
    2, -- D
    1, -- E
    4, -- F
    2, -- G
    4, -- H
    1, -- I
    8, -- J
    5, -- K
    1, -- L
    3, -- M
    1, -- N
    1, -- O
    3, -- P
    10, -- Q
    1, -- R
    1, -- S
    1, -- T
    1, -- U
    4, -- V
    4, -- W
    8, -- X
    4, -- Y
    10 -- Z
}

-- this sequence makes the scrabble tile values available at
-- indices corresponding to the letter's ASCII value for
-- both the uppercase and lowercase letters.
sequence values = repeat(0,127)
values['A'..'Z'] = tiles
values['a'..'z'] = tiles

public function score(sequence word)
    integer score = 0
    for i = 1 to length(word) do
        score += values[word[i]]
    end for
    return score
end function
