function Get-HammingDifference([string]$strand1, [string]$strand2) {
    $len = $strand1.Length
    if ($len -ne $strand2.Length) {
        throw "Left and right strands must be of equal length."
    }

    $distance = 0
    for ($i = 0; $i -lt $len; $i++) {
        if ($strand1[$i] -ne $strand2[$i]) {
            $distance++
        }
    }
    return $distance
}
