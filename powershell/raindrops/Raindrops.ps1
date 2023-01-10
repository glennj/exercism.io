Function Get-Raindrops() {
    [CmdletBinding()]
    Param(
        [int]$Rain
    )

    $sounds = ''

    if ( $Rain % 3 -eq 0 ) { $sounds += 'Pling' }
    if ( $Rain % 5 -eq 0 ) { $sounds += 'Plang' }
    if ( $Rain % 7 -eq 0 ) { $sounds += 'Plong' }

    if ( $sounds -eq '' ) { $sounds = $Rain }

    return $sounds
}
