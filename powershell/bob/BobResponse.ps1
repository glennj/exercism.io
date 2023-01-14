Function Get-BobResponse() {
    [CmdletBinding()]
    Param(
        [string]$HeyBob
    )

    $input = $HeyBob -replace '\s', ''

    if ($input -eq '') {
        return 'Fine. Be that way!'
    }
    else {
        $yelling  = ($input -cmatch '[A-Z]') -and -not ($input -cmatch '[a-z]')
        $question = $input -match '[?]$'

        if ($yelling -and $question) {
            return "Calm down, I know what I'm doing!"
        }
        elseif ($yelling) {
            return 'Whoa, chill out!'
        }
        elseif ($question) {
            return 'Sure.'
        }
        else {
            return 'Whatever.'
        }
    }
}
