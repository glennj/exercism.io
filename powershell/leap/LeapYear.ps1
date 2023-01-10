function Test-LeapYear {
    param( [int]$year )

    return $year % 4 -eq 0 -and 
            ($year % 100 -ne 0 -or $year % 400 -eq 0)
}
