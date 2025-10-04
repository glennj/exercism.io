const base_cars_per_hour = 221
const mins_per_hour = 60

function success_rate(speed)
    if speed <= 0
        0.0
    elseif speed <= 4
        1.0
    elseif speed <= 8
        0.90
    elseif speed <= 9
        0.80
    else
        0.77
    end
end

production_rate_per_hour(speed) = base_cars_per_hour * speed * success_rate(speed)    

working_items_per_minute(speed) = Int(production_rate_per_hour(speed) ÷ mins_per_hour)
