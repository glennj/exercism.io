using Dates

function add_gigasecond(date::DateTime)
    date + Second(10 ^ 9)
end
