today(birds_per_day) = birds_per_day[end]

function increment_todays_count(birds_per_day)
    today = pop!(birds_per_day)
    push!(birds_per_day, 1 + today)
end

has_day_without_birds(birds_per_day) = !isempty(birds_per_day[birds_per_day .== 0])

count_for_first_days(birds_per_day, num_days) = sum(birds_per_day[1:num_days])

busy_days(birds_per_day) = length(birds_per_day[birds_per_day .> 4])

average_per_day(week1, week2) = (week1 .+ week2) ./ 2
