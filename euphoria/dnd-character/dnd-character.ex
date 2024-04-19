include std/rand.e
include std/math.e
include std/search.e

-- -----------------------------------------
public function modifier(integer score)
  return floor((score - 10) / 2)
end function

public function ability()
  sequence rolls = {d6(), d6(), d6(), d6()}
  return sum(rolls) - min(rolls)
end function

function d6()
  return rand_range(1, 6)
end function

-- -----------------------------------------
enum STRENGTH, DEXTERITY, CONSTITUTION, INTELLIGENCE, WISDOM, CHARISMA, HITPOINTS
constant num_characteristics = 7

type dnd_character(sequence data)
  return length(data) = num_characteristics
    and integer(data[STRENGTH])     and is_in_range(data[STRENGTH], {3, 18})
    and integer(data[DEXTERITY])    and is_in_range(data[DEXTERITY], {3, 18})
    and integer(data[CONSTITUTION]) and is_in_range(data[CONSTITUTION], {3, 18})
    and integer(data[INTELLIGENCE]) and is_in_range(data[INTELLIGENCE], {3, 18})
    and integer(data[WISDOM])       and is_in_range(data[WISDOM], {3, 18})
    and integer(data[CHARISMA])     and is_in_range(data[CHARISMA], {3, 18})
    and integer(data[HITPOINTS])    and is_in_range(data[HITPOINTS], {6, 14})
end type

public function new_character()
  dnd_character c = repeat(6, num_characteristics)
  c[STRENGTH] = ability()
  c[DEXTERITY] = ability()
  c[CONSTITUTION] = ability()
  c[INTELLIGENCE] = ability()
  c[WISDOM] = ability()
  c[CHARISMA] = ability()
  c[HITPOINTS] = 10 + modifier(c[CONSTITUTION])
  return c
end function

public function get_strength(dnd_character character)
  return character[STRENGTH]
end function

public function get_dexterity(dnd_character character)
  return character[DEXTERITY]
end function

public function get_constitution(dnd_character character)
  return character[CONSTITUTION]
end function

public function get_intelligence(dnd_character character)
  return character[INTELLIGENCE]
end function

public function get_wisdom(dnd_character character)
  return character[WISDOM]
end function

public function get_charisma(dnd_character character)
  return character[CHARISMA]
end function

public function get_hitpoints(dnd_character character)
  return character[HITPOINTS]
end function