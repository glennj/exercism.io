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
