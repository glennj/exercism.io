get_vector_of_wagons(args...) = collect(args)  # or `[args...]`

function fix_vector_of_wagons(each_wagons_id, missing_wagons)
    penultimate, ultimate, engine, rest... = each_wagons_id
    [engine, missing_wagons..., rest..., penultimate, ultimate]
end

function add_missing_stops(route, stops...)
    Dict([route..., "stops" => [stop.second for stop in stops]])
    #
    # Note that `route["stops"] = ...` fails:
    # - route is a Dict{String,String}
    # - we can't add a pair with a value of type Vector{String}
end

function extend_route_information(route; more_route_information...)
    @show more_route_information
    #Dict([route..., more_route_information...])

    merge(route, more_route_information)
    #
    # Note that `merge!(route, more_route_information)` fails
    # - route is a Dict{String,String}
    # - more_ is a Base.Pairs{Symbol,String}
    #
    # `merge` is more lenient about types
end
