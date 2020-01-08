proc addGigasecond {datetime} {
    set fmt {%Y-%m-%dT%H:%M:%S}

    # use UTC time to avoid timezone issues.
    try {
        # try parsing as ISO-8601 point-in-time
        set time [clock scan $datetime -timezone :UTC -format $fmt]
    } on error {} {
        # else, free-form parsing
        set time [clock scan $datetime -timezone :UTC]
    }

    set time [clock add $time 1000000000 seconds]
    return [clock format $time -timezone :UTC -format $fmt]
}
