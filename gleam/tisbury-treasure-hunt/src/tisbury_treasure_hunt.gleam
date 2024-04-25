import gleam/pair

pub fn place_location_to_treasure_location(
  place_location: #(String, Int),
) -> #(Int, String) {
  // #(place_location.1, place_location.0)
  pair.swap(place_location)
}

pub fn treasure_location_matches_place_location(
  place_location: #(String, Int),
  treasure_location: #(Int, String),
) -> Bool {
  place_location_to_treasure_location(place_location) == treasure_location
}

pub fn count_place_treasures(
  place: #(String, #(String, Int)),
  treasures: List(#(String, #(Int, String))),
) -> Int {
  do_count_place_treasures(
    place_location_to_treasure_location(place.1),
    treasures,
    0,
  )
}

fn do_count_place_treasures(treasure_location, treasures, count) {
  case treasures {
    [] -> count
    [#(_, location), ..rest] if location == treasure_location ->
      do_count_place_treasures(treasure_location, rest, count + 1)
    [_, ..rest] -> do_count_place_treasures(treasure_location, rest, count)
  }
}

pub fn special_case_swap_possible(
  found_treasure: #(String, #(Int, String)),
  place: #(String, #(String, Int)),
  desired_treasure: #(String, #(Int, String)),
) -> Bool {
  // case found_treasure, place, desired_treasure {
  //   #("Brass Spyglass", _), #("Abandoned Lighthouse", _), _ -> True
  //   #("Amethyst Octopus", _), #("Stormy Breakwater", _), #(t, _) ->
  //     case t {
  //       "Crystal Crab" | "Glass Starfish" -> True
  //       _ -> False
  //     }
  //   #("Vintage Pirate Hat", _), #("Harbor Managers Office", _), #(t, _) ->
  //     case t {
  //       "Model Ship in Large Bottle" | "Antique Glass Fishnet Float" -> True
  //       _ -> False
  //     }
  //   _, _, _ -> False
  // }
  case found_treasure.0, place.0, desired_treasure.0 {
    "Brass Spyglass", "Abandoned Lighthouse", _ -> True
    "Amethyst Octopus", "Stormy Breakwater", "Crystal Crab" -> True
    "Amethyst Octopus", "Stormy Breakwater", "Glass Starfish" -> True
    "Vintage Pirate Hat", "Harbor Managers Office", "Model Ship in Large Bottle" -> True
    "Vintage Pirate Hat", "Harbor Managers Office", "Antique Glass Fishnet Float" -> True
    _, _, _ -> False
  }
}
