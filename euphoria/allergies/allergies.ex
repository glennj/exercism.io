include std/math.e

sequence allergens = {
    "eggs", "peanuts", "shellfish", "strawberries",
    "tomatoes", "chocolate", "pollen", "cats"
}

-- returns a true/false value
public function allergicTo(sequence allergen, integer score)
  return find(allergen, list(score))
end function

-- returns a sequence of strings
public function list(integer score)
  sequence allergies = {}
  for i = 1 to length(allergens) do
    if and_bits(shift_bits(score, i - 1), 1) then
      allergies = append(allergies, allergens[i])
    end if
  end for
  return allergies
end function
