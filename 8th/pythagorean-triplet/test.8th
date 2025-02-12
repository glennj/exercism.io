"pythagorean-triplet.8th" f:include
needs exercism/test
with: test
7 tests

\ Compare a1 to a2. Individual elements are compared with w (e.g., n:cmp).
\ The result n is:
\ - The first non-zero result of ( a1[i] a2[i] w ), or
\ - n:cmp of the lengths of a1 and a2
\ Note: This may be used in some test files. For example, to compare results
\ that are not required to be in a certain order.
: a:cmp SED: a1 a2 w -- n
    >r
    over a:len nip over a:len nip n:cmp
    true mark -rot \ Stack: length-cmp a1 a2
    ( r@ w:exec nip dup if break else drop then ) a:2each
    rdrop \ Done with comparison word
    2drop \ Done with a1 and a2
    mark?
    !if \ Got a non-zero result from a compare
        nip
    then
;

\ Helper function to allow triplets in any order
\ Sorts a1 before comparison
\ Expects a2 to be already sorted
: match-array? \ a1 a2 --
    ( swap ( ' n:cmp a:cmp ) a:sort ) equal?
;

"triplets whose sum is 12"
    12 triplets-with-sum
    [[3, 4, 5]]
    match-array?

SKIP-REST-OF-TESTS

"triplets whose sum is 108"
    108 triplets-with-sum
    [[27, 36, 45]]
    match-array?

"triplets whose sum is 1000"
    1000 triplets-with-sum
    [[200, 375, 425]]
    match-array?

"no matching triplets for 1001"
    1001 triplets-with-sum
    []
    match-array?

"returns all matching triplets"
    90 triplets-with-sum
    [[9, 40, 41], [15, 36, 39]]
    match-array?

"several matching triplets"
    840 triplets-with-sum
    [[40, 399, 401],  [56, 390, 394], [105, 360, 375], [120, 350, 370],
     [140, 336, 364], [168, 315, 357], [210, 280, 350], [240, 252, 348]]
    match-array?

"test triplets for large number"
    30_000 triplets-with-sum
    [[1200, 14375, 14425], [1875, 14000, 14125], [5000, 12000, 13000],
     [6000, 11250, 12750], [7500, 10000, 12500]]
    match-array?

\ 1 tests
\ "1 million, for extra credit"
\     1_000_000 triplets-with-sum
\     [[200000,375000,425000], [218750,360000,421250]]
\     match-array?

end-of-tests
;with
