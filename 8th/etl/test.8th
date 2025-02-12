"etl.8th" f:include
needs exercism/test
with: test
4 tests

"single letter"
    ( {"1": ["A"]} transform )
    {"a": 1}
    equal?

SKIP-REST-OF-TESTS

"single score with multiple letters"
    ( {"1": ["A", "E", "I", "O", "U"]} transform )
    {"a": 1, "e": 1, "i": 1, "o": 1, "u": 1}
    equal?

"multiple scores with multiple letters"
    ( {"1": ["A", "E"],
       "2": ["D", "G"]}
      transform )
    {"a": 1, "d": 2, "e": 1, "g": 2}
    equal?

"multiple scores with differing numbers of letters"
    ( {"1": ["A", "E", "I", "O", "U", "L", "N", "R", "S", "T"],
       "2": ["D", "G"],
       "3": ["B", "C", "M", "P"],
       "4": ["F", "H", "V", "W", "Y"],
       "5": ["K"],
       "8": ["J", "X"],
       "10": ["Q", "Z"]}
      transform )
    {"a": 1,
     "b": 3,
     "c": 3,
     "d": 2,
     "e": 1,
     "f": 4,
     "g": 2,
     "h": 4,
     "i": 1,
     "j": 8,
     "k": 5,
     "l": 1,
     "m": 3,
     "n": 1,
     "o": 1,
     "p": 3,
     "q": 10,
     "r": 1,
     "s": 1,
     "t": 1,
     "u": 1,
     "v": 4,
     "w": 4,
     "x": 8,
     "y": 4,
     "z": 10}
    equal?

end-of-tests
;with
