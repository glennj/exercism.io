namespace eval Allergies {
    namespace export allergicTo listAllergies

    variable allergens {
        eggs
        peanuts
        shellfish 
        strawberries 
        tomatoes 
        chocolate 
        pollen 
        cats 
    }

    proc allergicTo {allergen score} {
        expr {$allergen in [listAllergies $score]}
    }

    proc listAllergies {score} {
        variable allergens
        set allergies {}
        foreachWithIndex {i allergen} $allergens {
            if {($score & (1 << $i)) != 0} {
                lappend allergies $allergen
            }
        }
        return $allergies
    }
}

namespace import Allergies::*



proc foreachWithIndex {vars list body} {
    lassign $vars idxVar elemVar
    upvar 1 $idxVar idx
    upvar 1 $elemVar elem
    set len [llength $list]
    for {set idx 0} {$idx < $len} {incr idx} {
        set elem [lindex $list $idx]
        uplevel 1 $body
    }
}
