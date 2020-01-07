proc scrabbleScore {word} {
    set score 0
    foreach char [split [string toupper $word] ""] {
        switch -glob -- $char {
            [AEIOULNRST] { set tile 1 }
            [DG]         { set tile 2 }
            [BCMP]       { set tile 3 }
            [FHVWY]      { set tile 4 }
            [K]          { set tile 5 }
            [JX]         { set tile 8 }
            [QZ]         { set tile 10 }
            default      { set tile 0 }
        }
        incr score $tile
    }
    return $score
}
