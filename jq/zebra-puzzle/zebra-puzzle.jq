include "lib/permutations";

#  The zebra puzzle.
#
#   1. [-                  ] There are five houses.
#   2. [solveForNationality] The Englishman lives in the red house.
#   3. [solveForNationality] The Spaniard owns the dog.
#   4. [solveForBeverages  ] Coffee is drunk in the green house.
#   5. [solveForNationality] The Ukrainian drinks tea.
#   6. [solveForColour     ] The green house is immediately to the right of the ivory house.
#   7. [solveForPets       ] The Old Gold smoker owns snails.
#   8. [solveForCigarettes ] Kools are smoked in the yellow house.
#   9. [solveForBeverages  ] Milk is drunk in the middle house.
#  10. [solveForNationality] The Norwegian lives in the first house.
#  11. [solveForPets       ] The man who smokes Chesterfields lives in the house next to the man with the fox.
#  12. [solveForPets       ] Kools are smoked in the house next to the house where the horse is kept.
#  13. [solveForCigarettes ] The Lucky Strike smoker drinks orange juice.
#  14. [solveForNationality] The Japanese smokes Parliaments.
#  15. [solveForNationality] The Norwegian lives next to the blue house.

############################################################
def permute5: [1,2,3,4,5] | permutations;
def firstHouse: 1;
def middleHouse: 3;
def rightOf(a; b): a == b + 1;
def nextTo(a; b): rightOf(a;b) or rightOf(b;a);

############################################################
def solveForNationality($state):
  . as [$english, $spanish, $ukranian, $norwegian, $japanese]
  | if  $english == $state.red                          # clue 2
        and $spanish == $state.dog                      # clue 3
        and $ukranian == $state.tea                     # clue 5
        and $norwegian == firstHouse                    # clue 10
        and $japanese == $state.parliaments             # clue 14
        and nextTo($norwegian; $state.blue)             # clue 15
      then
        []
        | .[$english]   = "Englishman"
        | .[$spanish]   = "Spaniard"
        | .[$ukranian]  = "Ukranian"
        | .[$norwegian] = "Norwegian"
        | .[$japanese]  = "Japanese"
        | {
            drinksWater: .[$state.water],
            ownsZebra:   .[$state.zebra]
          }
      else
        empty
    end
;

############################################################
def solveForPets($state):
  . as [$dog, $snails, $fox, $horse, $zebra]
  | if  $snails == $state.oldGold                       # clue 7
        and nextTo($fox; $state.chesterfields)          # clue 11
        and nextTo($horse; $state.kools)                # clue 12
      then
        ($state + {$dog, $snails, $fox, $horse, $zebra}) as $state
        | permute5
        | solveForNationality($state)
      else
        empty
    end
;

############################################################
def solveForCigarettes($state):
  . as [$oldGold, $kools, $chesterfields, $luckyStrike, $parliaments]
  | if  $kools == $state.yellow                         # clue 8
        and $luckyStrike == $state.orangeJuice          # clue 13
      then
        ($state + {$oldGold, $kools, $chesterfields, $luckyStrike, $parliaments}) as $state
        | permute5
        | solveForPets($state)
      else
        empty
    end
;

############################################################
def solveForBeverages($state):
  . as [$coffee, $tea, $milk, $orangeJuice, $water]
  | if  $coffee == $state.green                         # clue 4
        and $milk == middleHouse                        # clue 9
      then
        ($state + {$coffee, $tea, $milk, $orangeJuice, $water}) as $state
        | permute5
        | solveForCigarettes($state)
      else
        empty
    end
;

############################################################
def solveForColour($state):
  . as [$red, $green, $ivory, $yellow, $blue]
  | if rightOf($green; $ivory)                          # clue 6
      then
        ($state + {$red, $green, $ivory, $yellow, $blue}) as $state
        | permute5
        | solveForBeverages($state)
      else
        empty
    end
;

############################################################
.property as $property
| permute5
| solveForColour({})
| .[$property]
