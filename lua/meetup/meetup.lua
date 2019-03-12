local starting_day

local meetup = function (params)
    assert(params and params.week and params.year and params.month and params.day)

    local t = os.time({
        year  = params.year,
        month = params.month,
        day   = starting_day(params),
    })

    -- now that we know the day this week starts, the
    -- desired day is somewhere in the next 7 days
    for i = 0, 6 do
        local wday, day = os.date("%A %d", t + i * 86400):match("(%a+) (%d+)")
        if wday == params.day then
            return tonumber(day)
        end
    end

    -- perhaps user mistyped day, or is in wrong locale
    error(('Cannot find %s %s'):format(params.last, params.day))
end

local START_DAY = {
    first  =  1,
    second =  8,
    third  = 15,
    fourth = 22,
    teenth = 13,
}
 
starting_day = function (params)
    if START_DAY[params.week] then
        return START_DAY[params.week]
    elseif params.week ~= "last" then
        error('Invalid week parameter')
    end

    -- strftime is perfectly happy handling
    --      2019-13-01
    -- as
    --      2020-01-01
    local first_day_of_next_month = os.time({
        year  = params.year,
        month = 1 + params.month,
        day   = 1,
    })
    -- subtract 7 days, ignoring DST
    return tonumber(os.date("%d", first_day_of_next_month - 86400 * 7))
end

return meetup
