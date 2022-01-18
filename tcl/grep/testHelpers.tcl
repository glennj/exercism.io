#############################################################
# Override some tcltest procs with additional functionality

# Allow an environment variable to override `skip`
proc skip {patternList} {
    if { [info exists ::env(RUN_ALL)]
         && [string is boolean -strict $::env(RUN_ALL)]
         && $::env(RUN_ALL)
    } then return else {
        uplevel 1 [list ::tcltest::skip $patternList]
    }
}

# Exit non-zero if any tests fail.
# The cleanupTests resets the numTests array, so capture it first.
proc cleanupTests {} {
    set failed [expr {$::tcltest::numTests(Failed) > 0}]
    uplevel 1 ::tcltest::cleanupTests
    if {$failed} then {exit 1}
}

#############################################################
# Testing data

set text(iliad.txt) [join {
    "Achilles sing, O Goddess! Peleus' son;"
    "His wrath pernicious, who ten thousand woes"
    "Caused to Achaia's host, sent many a soul"
    "Illustrious into Ades premature,"
    "And Heroes gave (so stood the will of Jove)"
    "To dogs and to all ravening fowls a prey,"
    "When fierce dispute had separated once"
    "The noble Chief Achilles from the son"
    "Of Atreus, Agamemnon, King of men."
} \n]

set text(midsummer-night.txt) [join {
    "I do entreat your grace to pardon me."
    "I know not by what power I am made bold,"
    "Nor how it may concern my modesty,"
    "In such a presence here to plead my thoughts;"
    "But I beseech your grace that I may know"
    "The worst that may befall me in this case,"
    "If I refuse to wed Demetrius."
} \n]

set text(paradise-lost.txt) [join {
    "Of Mans First Disobedience, and the Fruit"
    "Of that Forbidden Tree, whose mortal tast"
    "Brought Death into the World, and all our woe,"
    "With loss of Eden, till one greater Man"
    "Restore us, and regain the blissful Seat,"
    "Sing Heav'nly Muse, that on the secret top"
    "Of Oreb, or of Sinai, didst inspire"
    "That Shepherd, who first taught the chosen Seed"
} \n]

proc populateFiles {} {
    foreach {name contents} [array get ::text] {
        set fid [open $name w]
        puts $fid $contents
        close $fid
    }
}

proc removeFiles {} {
    foreach name [array names ::text] {
        file delete -force $name
    }
}
