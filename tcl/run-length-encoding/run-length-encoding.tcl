scan [info patchlevel] %f minorVer

if {$minorVer >= 8.7} {
    # Tcl 8.7 introduces a `-command` flag for regsub
    # See https://www.magicsplat.com/blog/tcl87-regsub/

    proc encode {string} {
        regsub -command -all {(.)\1+} $string {
            apply {{seq char} {string cat [string length $seq] $char}}
        }
    }

    proc decode {string} {
        regsub -command -all {(\d+)(.)} $string {
            apply {{- n char} {string repeat $char $n}}
        }
    }

} else {

    proc encode {string} {
        regsub -all {(.)\1+} $string {[string length "&"]\1} encoded
        # $encoded now contains embedded Tcl command substitutions: execute them
        subst -nobackslashes -novariables $encoded
    }

    proc decode {code} {
        regsub -all {(\d+)(.)} $code {[string repeat "\2" \1]} decoded
        subst -nobackslashes -novariables $decoded
    }

}
