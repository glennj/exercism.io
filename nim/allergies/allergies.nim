from strutils import parseEnum

type 
  Allergen = enum
    eggs
    peanuts
    shellfish
    strawberries
    tomatoes
    chocolate
    pollen
    cats

  Allergies* = object 
    score*: int


proc isAllergicTo(allergies: Allergies, allergen: Allergen): bool =
  (allergies.score and (1 shl ord(allergen))) != 0

proc isAllergicTo*(allergies: Allergies, allergen: string): bool =
  allergies.isAllergicTo(parseEnum[Allergen](allergen))


proc lst*(allergies: Allergies): seq[string] =
  for allergen in Allergen:
    if allergies.isAllergicTo(allergen):
      result.add($allergen)
