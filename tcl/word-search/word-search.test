#!/usr/bin/env tclsh
# generated: 2026-07-24T19:04:04Z
package require tcltest
namespace import ::tcltest::*
source testHelpers.tcl

# Uncomment next line to view test durations.
#configure -verbose {body error usec}

############################################################
source "word-search.tcl"

# note that grid coordinates are 1-indexed

test word-search-1 "Should accept an initial game grid and a target search word" -setup {
    set grid {
        "jefblpepre"
    }
} -body {
    wordSearch $grid {clojure}
} -returnCodes ok -match dictionary -result {
    "clojure" {}
}

skip word-search-2
test word-search-2 "Should locate one word written left to right" -setup {
    set grid {
        "clojurermt"
    }
} -body {
    wordSearch $grid {clojure}
} -returnCodes ok -match dictionary -result {
    "clojure" {{1 1} {7 1}}
}

skip word-search-3
test word-search-3 "Should locate the same word written left to right in a different position" -setup {
    set grid {
        "mtclojurer"
    }
} -body {
    wordSearch $grid {clojure}
} -returnCodes ok -match dictionary -result {
    "clojure" {{3 1} {9 1}}
}

skip word-search-4
test word-search-4 "Should locate a different left to right word" -setup {
    set grid {
        "coffeelplx"
    }
} -body {
    wordSearch $grid {coffee}
} -returnCodes ok -match dictionary -result {
    "coffee" {{1 1} {6 1}}
}

skip word-search-5
test word-search-5 "Should locate that different left to right word in a different position" -setup {
    set grid {
        "xcoffeezlp"
    }
} -body {
    wordSearch $grid {coffee}
} -returnCodes ok -match dictionary -result {
    "coffee" {{2 1} {7 1}}
}

skip word-search-6
test word-search-6 "Should locate a left to right word in two line grid" -setup {
    set grid {
        "jefblpepre"
        "tclojurerm"
    }
} -body {
    wordSearch $grid {clojure}
} -returnCodes ok -match dictionary -result {
    "clojure" {{2 2} {8 2}}
}

skip word-search-7
test word-search-7 "Should locate a left to right word in three line grid" -setup {
    set grid {
        "camdcimgtc"
        "jefblpepre"
        "clojurermt"
    }
} -body {
    wordSearch $grid {clojure}
} -returnCodes ok -match dictionary -result {
    "clojure" {{1 3} {7 3}}
}

skip word-search-8
test word-search-8 "Should locate a left to right word in ten line grid" -setup {
    set grid {
        "jefblpepre"
        "camdcimgtc"
        "oivokprjsm"
        "pbwasqroua"
        "rixilelhrs"
        "wolcqlirpc"
        "screeaumgr"
        "alxhpburyi"
        "jalaycalmp"
        "clojurermt"
    }
} -body {
    wordSearch $grid {clojure}
} -returnCodes ok -match dictionary -result {
    "clojure" {{1 10} {7 10}}
}

skip word-search-9
test word-search-9 "Should locate that left to right word in a different position in a ten line grid" -setup {
    set grid {
        "jefblpepre"
        "camdcimgtc"
        "oivokprjsm"
        "pbwasqroua"
        "rixilelhrs"
        "wolcqlirpc"
        "screeaumgr"
        "alxhpburyi"
        "clojurermt"
        "jalaycalmp"
    }
} -body {
    wordSearch $grid {clojure}
} -returnCodes ok -match dictionary -result {
    "clojure" {{1 9} {7 9}}
}

skip word-search-10
test word-search-10 "Should locate a different left to right word in a ten line grid" -setup {
    set grid {
        "jefblpepre"
        "camdcimgtc"
        "oivokprjsm"
        "pbwasqroua"
        "rixilelhrs"
        "wolcqlirpc"
        "fortranftw"
        "alxhpburyi"
        "clojurermt"
        "jalaycalmp"
    }
} -body {
    wordSearch $grid {fortran}
} -returnCodes ok -match dictionary -result {
    "fortran" {{1 7} {7 7}}
}

skip word-search-11
test word-search-11 "Should locate multiple words" -setup {
    set grid {
        "jefblpepre"
        "camdcimgtc"
        "oivokprjsm"
        "pbwasqroua"
        "rixilelhrs"
        "wolcqlirpc"
        "fortranftw"
        "alxhpburyi"
        "jalaycalmp"
        "clojurermt"
    }
} -body {
    wordSearch $grid {fortran clojure}
} -returnCodes ok -match dictionary -result {
    "clojure" {{1 10} {7 10}}
    "fortran" {{1 7} {7 7}}
}

skip word-search-12
test word-search-12 "Should locate a single word written right to left" -setup {
    set grid {
        "rixilelhrs"
    }
} -body {
    wordSearch $grid {elixir}
} -returnCodes ok -match dictionary -result {
    "elixir" {{6 1} {1 1}}
}

skip word-search-13
test word-search-13 "Should locate multiple words written in different horizontal directions" -setup {
    set grid {
        "jefblpepre"
        "camdcimgtc"
        "oivokprjsm"
        "pbwasqroua"
        "rixilelhrs"
        "wolcqlirpc"
        "screeaumgr"
        "alxhpburyi"
        "jalaycalmp"
        "clojurermt"
    }
} -body {
    wordSearch $grid {elixir clojure}
} -returnCodes ok -match dictionary -result {
    "clojure" {{1 10} {7 10}}
    "elixir" {{6 5} {1 5}}
}

skip word-search-14
test word-search-14 "Should locate words written top to bottom" -setup {
    set grid {
        "jefblpepre"
        "camdcimgtc"
        "oivokprjsm"
        "pbwasqroua"
        "rixilelhrs"
        "wolcqlirpc"
        "screeaumgr"
        "alxhpburyi"
        "jalaycalmp"
        "clojurermt"
    }
} -body {
    wordSearch $grid {clojure elixir ecmascript}
} -returnCodes ok -match dictionary -result {
    "clojure" {{1 10} {7 10}}
    "elixir" {{6 5} {1 5}}
    "ecmascript" {{10 1} {10 10}}
}

skip word-search-15
test word-search-15 "Should locate words written bottom to top" -setup {
    set grid {
        "jefblpepre"
        "camdcimgtc"
        "oivokprjsm"
        "pbwasqroua"
        "rixilelhrs"
        "wolcqlirpc"
        "screeaumgr"
        "alxhpburyi"
        "jalaycalmp"
        "clojurermt"
    }
} -body {
    wordSearch $grid {clojure elixir ecmascript rust}
} -returnCodes ok -match dictionary -result {
    "clojure" {{1 10} {7 10}}
    "elixir" {{6 5} {1 5}}
    "ecmascript" {{10 1} {10 10}}
    "rust" {{9 5} {9 2}}
}

skip word-search-16
test word-search-16 "Should locate words written top left to bottom right" -setup {
    set grid {
        "jefblpepre"
        "camdcimgtc"
        "oivokprjsm"
        "pbwasqroua"
        "rixilelhrs"
        "wolcqlirpc"
        "screeaumgr"
        "alxhpburyi"
        "jalaycalmp"
        "clojurermt"
    }
} -body {
    wordSearch $grid {clojure elixir ecmascript rust java}
} -returnCodes ok -match dictionary -result {
    "clojure" {{1 10} {7 10}}
    "elixir" {{6 5} {1 5}}
    "ecmascript" {{10 1} {10 10}}
    "rust" {{9 5} {9 2}}
    "java" {{1 1} {4 4}}
}

skip word-search-17
test word-search-17 "Should locate words written bottom right to top left" -setup {
    set grid {
        "jefblpepre"
        "camdcimgtc"
        "oivokprjsm"
        "pbwasqroua"
        "rixilelhrs"
        "wolcqlirpc"
        "screeaumgr"
        "alxhpburyi"
        "jalaycalmp"
        "clojurermt"
    }
} -body {
    wordSearch $grid {clojure elixir ecmascript rust java lua}
} -returnCodes ok -match dictionary -result {
    "clojure" {{1 10} {7 10}}
    "elixir" {{6 5} {1 5}}
    "ecmascript" {{10 1} {10 10}}
    "rust" {{9 5} {9 2}}
    "java" {{1 1} {4 4}}
    "lua" {{8 9} {6 7}}
}

skip word-search-18
test word-search-18 "Should locate words written bottom left to top right" -setup {
    set grid {
        "jefblpepre"
        "camdcimgtc"
        "oivokprjsm"
        "pbwasqroua"
        "rixilelhrs"
        "wolcqlirpc"
        "screeaumgr"
        "alxhpburyi"
        "jalaycalmp"
        "clojurermt"
    }
} -body {
    wordSearch $grid {clojure elixir ecmascript rust java lua lisp}
} -returnCodes ok -match dictionary -result {
    "clojure" {{1 10} {7 10}}
    "elixir" {{6 5} {1 5}}
    "ecmascript" {{10 1} {10 10}}
    "rust" {{9 5} {9 2}}
    "java" {{1 1} {4 4}}
    "lua" {{8 9} {6 7}}
    "lisp" {{3 6} {6 3}}
}

skip word-search-19
test word-search-19 "Should locate words written top right to bottom left" -setup {
    set grid {
        "jefblpepre"
        "camdcimgtc"
        "oivokprjsm"
        "pbwasqroua"
        "rixilelhrs"
        "wolcqlirpc"
        "screeaumgr"
        "alxhpburyi"
        "jalaycalmp"
        "clojurermt"
    }
} -body {
    wordSearch $grid {clojure elixir ecmascript rust java lua lisp ruby}
} -returnCodes ok -match dictionary -result {
    "clojure" {{1 10} {7 10}}
    "elixir" {{6 5} {1 5}}
    "ecmascript" {{10 1} {10 10}}
    "rust" {{9 5} {9 2}}
    "java" {{1 1} {4 4}}
    "lua" {{8 9} {6 7}}
    "lisp" {{3 6} {6 3}}
    "ruby" {{8 6} {5 9}}
}

skip word-search-20
test word-search-20 "Should fail to locate a word that is not in the puzzle" -setup {
    set grid {
        "jefblpepre"
        "camdcimgtc"
        "oivokprjsm"
        "pbwasqroua"
        "rixilelhrs"
        "wolcqlirpc"
        "screeaumgr"
        "alxhpburyi"
        "jalaycalmp"
        "clojurermt"
    }
} -body {
    wordSearch $grid {clojure elixir ecmascript rust java lua lisp ruby haskell}
} -returnCodes ok -match dictionary -result {
    "clojure" {{1 10} {7 10}}
    "elixir" {{6 5} {1 5}}
    "ecmascript" {{10 1} {10 10}}
    "rust" {{9 5} {9 2}}
    "java" {{1 1} {4 4}}
    "lua" {{8 9} {6 7}}
    "lisp" {{3 6} {6 3}}
    "ruby" {{8 6} {5 9}}
    "haskell" {}
}

skip word-search-21
test word-search-21 "Should fail to locate words that are not on horizontal, vertical, or diagonal lines" -setup {
    set grid {
        "abc"
        "def"
    }
} -body {
    wordSearch $grid {aef ced abf cbd}
} -returnCodes ok -match dictionary -result {
    "aef" {}
    "ced" {}
    "abf" {}
    "cbd" {}
}

skip word-search-22
test word-search-22 "Should not concatenate different lines to find a horizontal word" -setup {
    set grid {
        "abceli"
        "xirdfg"
    }
} -body {
    wordSearch $grid {elixir}
} -returnCodes ok -match dictionary -result {
    "elixir" {}
}

skip word-search-23
test word-search-23 "Should not wrap around horizontally to find a word" -setup {
    set grid {
        "silabcdefp"
    }
} -body {
    wordSearch $grid {lisp}
} -returnCodes ok -match dictionary -result {
    "lisp" {}
}

skip word-search-24
test word-search-24 "Should not wrap around vertically to find a word" -setup {
    set grid {
        "s"
        "u"
        "r"
        "a"
        "b"
        "c"
        "t"
    }
} -body {
    wordSearch $grid {rust}
} -returnCodes ok -match dictionary -result {
    "rust" {}
}


cleanupTests
