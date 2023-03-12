class LocomotiveEngineer
  def self.generate_list_of_wagons(*wagon_ids)
    wagon_ids
  end

  def self.fix_list_of_wagons(each_wagons_id, missing_wagons)
    a, b, loc, *wagons = each_wagons_id
    [loc, *missing_wagons, *wagons, a, b]
  end

  def self.add_missing_stops(routing, **missing_stops)
    # routing[:stops] = missing_stops.values
    # routing
    {**routing, stops: missing_stops.values}
  end

  def self.extend_route_information(route, more_route_information)
    {**route, **more_route_information}
  end
end
