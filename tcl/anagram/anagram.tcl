namespace eval Anagrams {
    proc findAnagrams {subject candidates} {
        set keyFunc {{word} {lsort [split $word ""]}}

        set lcSubj [string tolower $subject]
        set key [apply $keyFunc $lcSubj]

        set anagrams {}
        foreach candidate $candidates {
            set lc [string tolower $candidate]
            if {($lcSubj ne $lc) && ($key eq [apply $keyFunc $lc])} {
                lappend anagrams $candidate
            }
        }
        return $anagrams
    }

    # or, with math! https://twitter.com/fermatslibrary/status/1275066521450975234
    proc findAnagramsByHashing {subject candidates} {
        set lcSubject [string tolower $subject]
        set key [hash $lcSubject]
        lmap candidate $candidates {
            set lc [string tolower $candidate]
            if {($lcSubject eq $lc) || ($key != [hash $lc])} {
                continue
            }
            set candidate
        }
    }

    # first 26 prime numbers
    variable primeMap {
        a "*   2"   j "*  29"   s "*  67"
        b "*   3"   k "*  31"   t "*  71"
        c "*   5"   l "*  37"   u "*  73"
        d "*   7"   m "*  41"   v "*  79"
        e "*  11"   n "*  43"   w "*  83"
        f "*  13"   o "*  47"   x "*  89"
        g "*  17"   p "*  53"   y "*  97"
        h "*  19"   q "*  59"   z "* 101"
        i "*  23"   r "*  61"
    }

    proc hash {word} {
        variable primeMap
        expr [string cat 1 [string map $primeMap [regsub -all {[^[:alpha:]]} $word ""]]]
    }
}

#interp alias {} findAnagrams {} Anagrams::findAnagrams
interp alias {} findAnagrams {} Anagrams::findAnagramsByHashing
