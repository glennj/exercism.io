namespace eval ProteinTranslation {
    namespace export proteins

    variable c2p {
        UUU Phenylalanine   AUG Methionine
        UUC Phenylalanine   UAU Tyrosine
        UUA Leucine         UAC Tyrosine
        UUG Leucine         UGU Cysteine
        UCU Serine          UGC Cysteine
        UCC Serine          
        UCA Serine          UAA STOP
        UCG Serine          UAG STOP
        UGG Tryptophan      UGA STOP
    }

    proc proteins {rna} {
        variable c2p

        set proteins {}
        while {[string length $rna] > 0} {
            set codon [popCodon rna]
            assert {[dict exists $c2p $codon]} "Invalid codon"

            set protein [dict get $c2p $codon]
            if {$protein eq "STOP"} then break

            lappend proteins $protein
        }

        ## alternately, with `lmap` and `regexp`
        #set proteins [lmap codon [regexp -inline -all {.{1,3}} $rna] {
        #    assert {[dict exists $c2p $codon]} "Invalid codon"
        #    set protein [dict get $c2p $codon]
        #    if {$protein eq "STOP"} then break else {set protein}
        #}]

        return $proteins
    }

    proc popCodon {varname} {
        upvar 1 $varname strand
        set codon  [string range $strand 0 2]
        set strand [string range $strand 3 end]
        return $codon
    }
}

namespace import ProteinTranslation::proteins


# throw an error if the condition is false
proc assert {condition errorMessage} {
    if {![uplevel 1 [list expr $condition]]} {
        return -code error $errorMessage
    }
}
