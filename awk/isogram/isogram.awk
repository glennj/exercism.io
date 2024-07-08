BEGIN { FS = "" }     # each character is a separate field.

{
    # remove non-letters
    $0 = gensub(/[^[:alpha:]]/, "", "g", tolower($0))
    
    delete seen
    result = "true"
    for (i = 1; i <= NF; i++) {
        if (seen[$i]++) {
            result = "false"
            break
        }
    }
    print result
}
