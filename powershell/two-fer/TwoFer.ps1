Function Get-TwoFer(){
    [CmdletBinding()]
    Param(
        #[Parameter(Mandatory=$true, Position=0)]
        [Parameter(Position=0)]
        [string]$Name
    )

    #if ( $Name -eq "" -or $Name -eq $null ) { $Name = "you" }
    if ( [string]::IsNullOrEmpty($Name) ) { $Name = 'you' }

    return "One for $Name, one for me"
}
