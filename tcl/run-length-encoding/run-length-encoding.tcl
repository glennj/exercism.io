proc encode {string} {
    regsub -all {(.)\1+} $string {[string length "&"]\1} encoded
    # $encoded now contains embedded Tcl command substitutions: execute them
    subst -nobackslashes -novariables $encoded
}

proc decode {code} {
    regsub -all {(\d+)(.)} $code {[string repeat "\2" \1]} decoded
    subst -nobackslashes -novariables $decoded
}
