get_coordinate(pair) = pair[2]

convert_coordinate(coordinate) = Tuple(coordinate)

# azara_record is (treasure, coord_string)
# rui_record is (location, coord_tuple, quadrant)
function compare_records(azara_record, rui_record)
    convert_coordinate(get_coordinate(azara_record)) == rui_record[2]    
end

function create_record(azara_record, rui_record)
    if !compare_records(azara_record, rui_record)
        return ()
    end

    treasure, coordinate = azara_record
    location, _, quadrant = rui_record
    (coordinate, location, quadrant, treasure)
end
