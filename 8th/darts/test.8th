"darts.8th" f:include
needs exercism/test
with: test
13 tests

"Missed target"
    ( -9 9 darts-score )
    0
    equal?

SKIP-REST-OF-TESTS

"On the outer circle"
    ( 0 10 darts-score )
    1
    equal?

"On the middle circle"
    ( -5 0 darts-score )
    5
    equal?

"On the inner circle"
    ( 0 -1 darts-score )
    10
    equal?

"Exactly on center"
    ( 0 0 darts-score )
    10
    equal?

"Near the center"
    ( -0.1 -0.1 darts-score )
    10
    equal?

"Just within the inner circle"
    ( 0.7 0.7 darts-score )
    10
    equal?

"Just outside the inner circle"
    ( 0.8 -0.8 darts-score )
    5
    equal?

"Just within the middle circle"
    ( -3.5 3.5 darts-score )
    5
    equal?

"Just outside the middle circle"
    ( -3.6 -3.6 darts-score )
    1
    equal?

"Just within the outer circle"
    ( -7.0 7.0 darts-score )
    1
    equal?

"Just outside the outer circle"
    ( 7.1 -7.1 darts-score )
    0
    equal?

"Asymmetric position between the inner and middle circles"
    ( 0.5 -4 darts-score )
    5
    equal?

end-of-tests
;with
