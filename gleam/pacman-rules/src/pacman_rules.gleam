pub fn eat_ghost(power_pellet_active: Bool, touching_ghost: Bool) -> Bool {
  power_pellet_active && touching_ghost
}

pub fn score(touching_power_pellet: Bool, touching_dot: Bool) -> Bool {
  touching_power_pellet || touching_dot
}

pub fn lose(power_pellet_active: Bool, touching_ghost: Bool) -> Bool {
  touching_ghost && !power_pellet_active
}

pub fn win(
  has_eaten_all_dots: Bool,
  power_pellet_active: Bool,
  touching_ghost: Bool,
) -> Bool {
  has_eaten_all_dots && !lose(power_pellet_active, touching_ghost)
}
