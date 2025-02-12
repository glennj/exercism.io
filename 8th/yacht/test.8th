"yacht.8th" f:include
needs exercism/test
with: test
29 tests

"Yacht"
    ( [5, 5, 5, 5, 5] "yacht"  play_yacht )
    50
    equal?

SKIP-REST-OF-TESTS

"Not Yacht"
    ( [1, 3, 3, 2, 5] "yacht"  play_yacht )
    0
    equal?

"Ones"
    ( [1, 1, 1, 3, 5] "ones"  play_yacht )
    3
    equal?

"Ones, out of order"
    ( [3, 1, 1, 5, 1] "ones"  play_yacht )
    3
    equal?

"No ones"
    ( [4, 3, 6, 5, 5] "ones"  play_yacht )
    0
    equal?

"Twos"
    ( [2, 3, 4, 5, 6] "twos"  play_yacht )
    2
    equal?

"Yacht counted as threes"
    ( [3, 3, 3, 3, 3] "threes"  play_yacht )
    15
    equal?

"Fours"
    ( [1, 4, 1, 4, 1] "fours"  play_yacht )
    8
    equal?

"Yacht of 3s counted as fives"
    ( [3, 3, 3, 3, 3] "fives"  play_yacht )
    0
    equal?

"Fives"
    ( [1, 5, 3, 5, 3] "fives"  play_yacht )
    10
    equal?

"Sixes"
    ( [2, 3, 4, 5, 6] "sixes"  play_yacht )
    6
    equal?

"Four of a Kind"
    ( [6, 6, 4, 6, 6] "four of a kind"  play_yacht )
    24
    equal?

"Yacht can be scored as Four of a Kind"
    ( [3, 3, 3, 3, 3] "four of a kind"  play_yacht )
    12
    equal?

"Full house is not Four of a Kind"
    ( [3, 3, 3, 5, 5] "four of a kind"  play_yacht )
    0
    equal?

"Little Straight"
    ( [3, 5, 4, 1, 2] "little straight"  play_yacht )
    30
    equal?

"Little Straight as Big Straight"
    ( [1, 2, 3, 4, 5] "big straight"  play_yacht )
    0
    equal?

"Four in order but not a little straight"
    ( [1, 1, 2, 3, 4] "little straight"  play_yacht )
    0
    equal?

"No pairs but not a little straight"
    ( [1, 2, 3, 4, 6] "little straight"  play_yacht )
    0
    equal?

"Minimum is 1, maximum is 5, but not a little straight"
    ( [1, 1, 3, 4, 5] "little straight"  play_yacht )
    0
    equal?

"Big Straight"
    ( [4, 6, 2, 5, 3] "big straight"  play_yacht )
    30
    equal?

"Big Straight as little straight"
    ( [6, 5, 4, 3, 2] "little straight"  play_yacht )
    0
    equal?

"No pairs but not a big straight"
    ( [6, 5, 4, 3, 1] "big straight"  play_yacht )
    0
    equal?

"Full house two small, three big"
    ( [2, 2, 4, 4, 4] "full house"  play_yacht )
    16
    equal?

"Full house three small, two big"
    ( [5, 3, 3, 5, 3] "full house"  play_yacht )
    19
    equal?

"Two pair is not a full house"
    ( [2, 2, 4, 4, 5] "full house"  play_yacht )
    0
    equal?

"Four of a kind is not a full house"
    ( [1, 4, 4, 4, 4] "full house"  play_yacht )
    0
    equal?

"Yacht is not a full house"
    ( [2, 2, 2, 2, 2] "full house"  play_yacht )
    0
    equal?

"Choice"
    ( [3, 3, 5, 6, 6] "choice"  play_yacht )
    23
    equal?

"Yacht as choice"
    ( [2, 2, 2, 2, 2] "choice"  play_yacht )
    10
    equal?

end-of-tests
;with
