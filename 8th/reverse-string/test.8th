"reverse-string.8th" f:include
needs exercism/test
with: test
6 tests

"an empty string"
    ( "" reverse )
    ""
    equal?

SKIP-REST-OF-TESTS

"a word"
    ( "robot" reverse )
    "tobor"
    equal?

"a capitalized word"
    ( "Ramen" reverse )
    "nemaR"
    equal?

"a sentence with punctuation"
    ( "I'm hungry!" reverse )
    "!yrgnuh m'I"
    equal?

"a palindrome"
    ( "racecar" reverse )
    "racecar"
    equal?

"an even-sized word"
    ( "drawer" reverse )
    "reward"
    equal?

end-of-tests
;with