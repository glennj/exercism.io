local baffling_birthdays = {}

baffling_birthdays.shared_birthday = function(birthdates)
    local seen = {}
    for _, bd in ipairs(birthdates) do
        md = bd:sub(6)
        if seen[md] then return true end
        seen[md] = true
    end
    return false
end

local function is_leap_year(year)
    return year % 4 == 0 and (year % 100 ~= 0 or year % 400 == 0)
end

local function random_birthdate()
    local year = 4
    while is_leap_year(year) do
        year = 1900 + math.random(120)
    end
    local time = os.time({year = year, month = 1, day = math.random(365)})
    return os.date('%Y-%m-%d', time)
end

baffling_birthdays.random_birthdates = function(count)
    local dates = {}
    for _ = 1, count do
        dates[#dates + 1] = random_birthdate()
    end
    return dates
end

baffling_birthdays.estimated_probability_of_shared_birthday = function(group_size)
    -- determine the probability that there is a shared birthday amongst n people
    -- https://en.wikipedia.org/wiki/Birthday_problem#Approximations
    return  100 * (1 - math.exp(-group_size * (group_size - 1) / 730))
end

return baffling_birthdays
