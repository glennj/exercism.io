const mix_times = Dict(
    "Pure Strawberry Joy" => 0.5,
    "Energizer" => 1.5,
    "Green Garden" => 1.5,
    "Tropical Island" => 3.0,
    "All or Nothing" => 5.0
)
const default_mix_time = 2.5
    
time_to_mix_juice(juice) = get(mix_times, juice, default_mix_time)
wedges_from_lime(size)   = get(Dict("small" => 6, "medium" => 8, "large" => 10), size, 0)

function limes_to_cut(needed, limes)
    i = 0
    while needed > 0 && i < length(limes)
        i += 1
        needed -= wedges_from_lime(limes[i])
    end
    return i
end

# order_times(orders) = map(time_to_mix_juice, orders)
# or, practice this concept
function order_times(orders)
    times = []
    for juice in orders
        push!(times, time_to_mix_juice(juice))
    end
    times
end

function remaining_orders(time_left, orders)
    while time_left > 0 && !isempty(orders)
        time_left -= time_to_mix_juice(popfirst!(orders))
    end
    orders
end
