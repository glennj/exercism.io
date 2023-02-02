# Exercism Powershell track

## Hello World

* `> Install-Module -Name Pester -Force -SkipPublisherCheck`
    * failed with lack of Admin privs
    * installed as Admin
    * then noticed the "by adding -Scope CurrentUser" message and did that too
* `> Invoke-Pester`
    * "Invoke-Pester : The 'Invoke-Pester' command was found in the module 'Pester', but the module could not be loaded. For more information, run 'Import-Module Pester'."
* `> Import-Module Pester`
    * "Import-Module : File C:\Program Files\WindowsPowerShell\Modules\Pester\5.4.0\Pester.psm1 cannot be loaded because running scripts is disabled on this system. For more information, see about_Execution_Policies at https:/go.microsoft.com/fwlink/?LinkID=135170."
    * started PS as Admin
        * `> Get-ExecutionPolicy` => "Restricted"
        * `> Set-ExecutionPolicy`:
            ```
            cmdlet Set-ExecutionPolicy at command pipeline position 1
            Supply values for the following parameters:
            ExecutionPolicy: Unrestricted
            ```
        * `> Get-ExecutionPolicy` => "Unrestricted"
* `> Invoke-Pester`
    * "... Tests Passed: 0, Failed: 1, Skipped: 0 NotRun: 0"
* edit HelloWorld.ps1
* `> Invoke-Pester`
    * "... Tests Passed: 1, Failed: 0, Skipped: 0 NotRun: 0"


## Two Fer

* [quoting rules](https://learn.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_quoting_rules?view=powershell-7.3)
* [If](https://learn.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_if?view=powershell-7.3)
* key to exercise: remove `Mandatory` attribute from [parameter config](https://learn.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_functions_advanced_parameters?view=powershell-7.3).

## Leap

* [comparison operators](https://learn.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_comparison_operators?view=powershell-7.3)

## Collatz Conjecture

* [while](https://learn.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_while?view=powershell-7.3)

## Raindrops

## Reverse String

* [split a string](https://learn.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_split?view=powershell-7.3)
* [foreach](https://learn.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_foreach?view=powershell-7.3)
