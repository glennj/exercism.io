namespace eval TicTacToe {
    namespace export gamestate

    proc gamestate {board} {
        # express the board as a base-2 number for X and for O
        set b [string cat {*}$board]
        set X 0b[string map {X 1 O 0 " " 0} $b]
        set O 0b[string map {O 1 X 0 " " 0} $b]

        set xWin [won? $X]
        set oWin [won? $O]
        if {$xWin && $oWin} {
            error "Impossible board: game should have ended after the game was won"
        }
        if {$xWin || $oWin} {
            return win
        }

        set bitDiff [expr {[bitCount $X] - [bitCount $O]}]  ;# should be 0 or 1
        if {$bitDiff > 1} {
            error "Wrong turn order: X went twice"
        }
        if {$bitDiff < 0} {
            error "Wrong turn order: O started"
        }

        return [expr {($X | $O) == 0b111111111 ? "draw" : "ongoing"}]
    }

    variable winningScores {
        0b111000000
        0b000111000
        0b000000111
        0b100100100
        0b010010010
        0b001001001
        0b100010001
        0b001010100
    }

    proc won? {score} {
        variable winningScores
        foreach w $winningScores {
            if {($score & $w) == $w} {
                return true
            }
        }
        return false
    }

    proc bitCount {n} {
        set count 0
        while {$n > 0} {
            incr count [expr {$n & 1}]
            set n [expr {$n >> 1}]
        }
        return $count
    }
}

namespace import TicTacToe::gamestate
