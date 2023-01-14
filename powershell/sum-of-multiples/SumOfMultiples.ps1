Function Get-SumOfMultiples {
    [CmdletBinding()]
    Param(
        [int[]]$Multiples,
        [int]$Limit
    )

    <#
    $set = @{}
    foreach ($m in $Multiples) {
        for ($i = $m; $i -lt $Limit; $i += $m) {
            $set[$i] = 1
        }
    }

    $sum = 0
    foreach ($n in $set.Keys) {
        $sum += $n
    }
    return $sum
    #>

    # thanks to https://exercism.org/tracks/powershell/exercises/sum-of-multiples/solutions/GarySGlover

    $factors = `
        foreach ($m in $Multiples) {
            for ($i = $m; $i -lt $Limit; $i += $m) {
                $i
            }
        }
    $factors | Select-Object -Unique `
             | Measure-Object -Sum `
             | Select-Object -ExpandProperty Sum
}
