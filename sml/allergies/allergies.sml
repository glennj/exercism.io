datatype allergen = Eggs
                  | Peanuts
                  | Shellfish
                  | Strawberries
                  | Tomatoes
                  | Chocolate
                  | Pollen
                  | Cats

local
  val allergens = Array.fromList [ Eggs, Peanuts, Shellfish, Strawberries
                                 , Tomatoes, Chocolate, Pollen, Cats
                                 ]
  fun index a =
    case Array.findi (fn (_, v) => a = v) allergens
      of SOME (i, _) => i
       | NONE        => raise Domain

  infix >>
  fun (a >> b) = Word.>>(Word.fromInt a, Word.fromInt b)
  infix &
  fun (a & b) = Word.andb(a, b)

in
  fun allergicTo (score: int) (a: allergen): bool =
    ((score >> index a) & 0w1) = 0w1

  fun allergies (score: int): allergen list =
    Array.foldr (fn (a, l) => if allergicTo score a then a::l else l) [] allergens
end
