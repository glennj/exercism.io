Function Get-ReverseString {
    [CmdletBinding()]
    Param(
        [Parameter(Position=1, ValueFromPipeline=$true)]
        [string]$Forward
    )
    
    $reversed = ''

    foreach ($char in ($Forward -split '')) {
        $reversed = $char + $reversed
    }

    return $reversed
}
