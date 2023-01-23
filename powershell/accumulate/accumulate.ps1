Function Get-Accumulation() {
    [CmdletBinding()]
    Param(
        [PSObject[]]$List,
        [scriptblock]$Func
    )
    
# TODO: last test failing...

#    $List | ForEach-Object {
#        Invoke-Command -Scriptblock $Func -ArgumentList $_
#    }

    $result = @()

    foreach ($item in $List) {
        $result += Invoke-Command -Scriptblock $Func -ArgumentList $item
    }

    return $result
}
