Function Invoke-CollatzConjecture() {
    [CmdletBinding()]
    Param(
        [Int64]$Number
    )

    if ( $Number -le 0 ) {
        Throw "error: Only positive numbers are allowed"
    }

    $steps = 0

    While ( $Number -gt 1 ) {
        if ( $Number % 2 -eq 0 ) {
            $Number /= 2
        }
        else {
            $Number = 3 * $Number + 1
        }
        $steps++
    }

    return $steps
}
