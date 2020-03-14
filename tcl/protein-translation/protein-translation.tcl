namespace eval ProteinTranslation {
    namespace export proteins

    variable codons {
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

    proc proteins {strand} {
        variable codons
        lmap codon [regexp -inline -all {...} $strand] {
            set protein [dict getdef $codons $codon "STOP"]
            if {$protein eq "STOP"} then break else {set protein}
        }
    }
}

namespace import ProteinTranslation::*


# Monkeypatching a new subcommand into the builtin `dict` command.
# `dict getdef` will be in Tcl 8.7
#
proc ::tcl::dict::getdef {dictValue key default} {
    if {[dict exists $dictValue $key]} {
        return [dict get $dictValue $key]
    } else {
        return $default
    }
}
namespace ensemble configure dict -map [concat \
    [namespace ensemble configure dict -map] \
    getdef ::tcl::dict::getdef ]
