Function Get-NucleotideCount() {
    [CmdletBinding()]
    Param(
        [string]$Strand
    )

    $nucleotides = "ACGT".ToCharArray()
    $count = @{}

    foreach ($char in $Strand.ToCharArray()) {
        if ($char -notin $nucleotides) {
            throw "Invalid nucleotide"
        }
        $count[$char]++
    }

    $pairs = `
        foreach ($nucleotide in $nucleotides) {
           if ($nucleotide -notin $count.Keys) {
                $count[$nucleotide] = 0
            }
            "{0}:{1}" -f $nucleotide, $count[$nucleotide]
        }
    return $pairs -join " "
}
