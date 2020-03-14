namespace eval PigLatin {
    namespace export translate

    proc translate {sentence} {
        set words [regexp -inline -all {\S+} $sentence]
        set translated [lmap word $words {translateWord $word}]
        return [join $translated]
    }

    proc translateWord {word} {
        foreach pattern {
            {^ () ((?: [aeiou] | xr | yt).*)    # apple, xray, ytrium }
            {^ ([^aeiou]?qu) (.*)   # queen, squeeze }
            {^ ([^aeiou]+)   (y.*)  # my, rhythm }
            {^ ([^aeiou]+)   (.*)   # strength }
        } {
            if {[regsub -expanded $pattern $word {\2\1ay} ordway]} {
                return $ordway
            }
        }

        # have we missed any case?
        return "${word}ay"
    }
}

namespace import PigLatin::*
