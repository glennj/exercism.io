proc scrabbleScore {word} {
    set score 0
    foreach char [split [string toupper $word] ""] {
        incr score [switch -glob -- $char {
            [AEIOULNRST] { expr 1 }
            [DG]         { expr 2 }
            [BCMP]       { expr 3 }
            [FHVWY]      { expr 4 }
            [K]          { expr 5 }
            [JX]         { expr 8 }
            [QZ]         { expr 10 }
            default      { expr 0 }
        }]
    }
    return $score
}
