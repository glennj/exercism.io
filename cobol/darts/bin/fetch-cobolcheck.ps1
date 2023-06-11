# This file is a copy of the
# https://github.com/exercism/configlet/blob/main/scripts/fetch-configlet.ps1 file.
# Please submit bugfixes/improvements to the above file to ensure that all tracks
# benefit from the changes.

$ErrorActionPreference = "Stop"
$ProgressPreference = "SilentlyContinue"

$requestOpts = @{
    Headers           = If ($env:GITHUB_TOKEN) { @{ Authorization = "Bearer ${env:GITHUB_TOKEN}" } } Else { @{ } }
    MaximumRetryCount = 3
    RetryIntervalSec  = 1
}

$arch = If ([Environment]::Is64BitOperatingSystem) { "amd64" } Else { "x86" }
$fileName = "cobol-check-windows-$arch.exe"

Function Get-DownloadUrl {
    $latestUrl = "https://api.github.com/repos/0xE282B0/cobol-check/releases/latest"
    Invoke-RestMethod -Uri $latestUrl -PreserveAuthorizationOnRedirect @requestOpts
    | Select-Object -ExpandProperty assets
    | Where-Object { $_.browser_download_url -match $FileName }
    | Select-Object -ExpandProperty browser_download_url
}

$downloadUrl = Get-DownloadUrl
$outputFile = Join-Path -Path $PSScriptRoot -ChildPath "cobolcheck.exe"
Invoke-WebRequest -Uri $downloadUrl -OutFile $outputFile @requestOpts