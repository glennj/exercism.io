import tables

type
  Dice = CountTable[int]

  YachtScores* = enum
    Ones = 1, Twos, Threes, Fours, Fives, Sixes,
    FullHouse, FourOfAKind, LittleStraight, BigStraight,
    Yacht, Choice


proc sum(dice: Dice): int =
  for die, count in dice:
    result.inc die * count

proc scoreFor(dice: Dice, die: int): int =
  dice.getOrDefault(die, 0) * die

proc fullHouse(dice: Dice): int =
  if dice.len == 2:
    for die, count in dice:
      if count in {2, 3}:
        result.inc die * count

proc fourKind(dice: Dice): int =
  for die, count in dice:
    if count in {4, 5}:
      return die * 4

proc straight(dice: Dice; a, b: int): int =
  result = 30
  for die in a..b:
    if die notin dice:
      return 0

proc yacht(dice: Dice): int =
  result = 50
  if dice.len > 1: return 0


proc score*(rolls: array[5, int], category: YachtScores): int =
  var dice: Dice = rolls.toCountTable
  case category
    of Ones..Sixes: dice.scoreFor category.ord
    of FullHouse: dice.fullHouse
    of FourOfAKind: dice.fourKind
    of LittleStraight: dice.straight 1, 5
    of BigStraight: dice.straight 2, 6
    of Yacht: dice.yacht
    of Choice: dice.sum
