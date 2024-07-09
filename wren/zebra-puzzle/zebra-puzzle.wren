import "./permutater" for Permutater

class ZebraPuzzle {
  construct new() {
    _perm = Permutater.new([1, 2, 3, 4, 5])                  // clue 1
    _FIRST = 1
    _MIDDLE = 3
    _houseOf = solveForColours({})
  }

  drinksWater { _nationalities[_houseOf["water"]] }
  ownsZebra   { _nationalities[_houseOf["zebra"]] }

  rightOf(a, b) { a + 1 == b }
  nextTo(a, b)  { rightOf(a, b) || rightOf(b, a) }

  solveForColours(state) {
    for (perm in _perm.permutations) {
      state["red"]    = perm[0]
      state["green"]  = perm[1]
      state["blue"]   = perm[2]
      state["yellow"] = perm[3]
      state["ivory"]  = perm[4]

      if (rightOf(state["green"], state["ivory"]) &&         // clue 6
          solveForNationalities(state)) {
        return state
      }
    }
  }

  solveForNationalities(s) {
    for (perm in _perm.permutations) {
      s["en"] = perm[0]
      s["sp"] = perm[1]
      s["uk"] = perm[2]
      s["nw"] = perm[3]
      s["jp"] = perm[4]

      _nationalities = {
        s["en"]: "Englishman",
        s["sp"]: "Spaniard",
        s["uk"]: "Ukranian",
        s["nw"]: "Norwegian",
        s["jp"]: "Japanese"
      }

      if (s["en"] == s["red"] &&                             // clue 2
          s["nw"] == _FIRST &&                               // clue 10
          nextTo(s["nw"], s["blue"]) &&                      // clue 15
          solveForDrinks(s)) {
        return s
      }
    }
  }

  solveForDrinks(s) {
    for (perm in _perm.permutations) {
      s["coffee"] = perm[0]
      s["tea"]    = perm[1]
      s["milk"]   = perm[2]
      s["juice"]  = perm[3]
      s["water"]  = perm[4]

      if (s["coffee"] == s["green"] &&                       // clue 4
          s["uk"] == s["tea"] &&                             // clue 5
          s["milk"] == _MIDDLE &&                            // clue 9
          solveForHobbies(s)) {
        return s
      }
    }
  }

  solveForHobbies(s) {
    for (perm in _perm.permutations) {
      s["paint"]    = perm[0]
      s["dance"]    = perm[1]
      s["read"]     = perm[2]
      s["football"] = perm[3]
      s["chess"]    = perm[4]

      if (s["paint"] == s["yellow"] &&                       // clue 8
          s["football"] == s["juice"] &&                     // clue 13
          s["jp"] == s["chess"] &&                           // clue 14
          solveForPets(s)) {
        return s
      }
    }
  }

  solveForPets(s) {
    for (perm in _perm.permutations) {
      s["dog"]   = perm[0]
      s["snail"] = perm[1]
      s["fox"]   = perm[2]
      s["horse"] = perm[3]
      s["zebra"] = perm[4]

      if (s["sp"] == s["dog"] &&                                  // clue 3
          s["dance"] == s["snail"] &&                             // clue 7
          nextTo(s["read"], s["fox"]) &&                          // clue 11
          nextTo(s["paint"], s["horse"])) {                       // clue 12
        return s
      }
    }
  }
}
