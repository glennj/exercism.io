import "./state-of-tic-tac-toe" for StateOfTicTacToe
import "wren-testie/testie" for Testie, Expect

Testie.test("StateOfTicTacToe") { |do, skip|
  do.test("Won games -> Finished game where X won via left column victory") {
    Expect.value(StateOfTicTacToe.gamestate([
      "XOO",
      "X  ",
      "X  "
    ])).toEqual("win")
  }

  do.test("Won games -> Finished game where X won via middle column victory") {
    Expect.value(StateOfTicTacToe.gamestate([
      "OXO",
      " X ",
      " X "
    ])).toEqual("win")
  }

  do.test("Won games -> Finished game where X won via right column victory") {
    Expect.value(StateOfTicTacToe.gamestate([
      "OOX",
      "  X",
      "  X"
    ])).toEqual("win")
  }

  do.test("Won games -> Finished game where O won via left column victory") {
    Expect.value(StateOfTicTacToe.gamestate([
      "OXX",
      "OX ",
      "O  "
    ])).toEqual("win")
  }

  do.test("Won games -> Finished game where O won via middle column victory") {
    Expect.value(StateOfTicTacToe.gamestate([
      "XOX",
      " OX",
      " O "
    ])).toEqual("win")
  }

  do.test("Won games -> Finished game where O won via right column victory") {
    Expect.value(StateOfTicTacToe.gamestate([
      "XXO",
      " XO",
      "  O"
    ])).toEqual("win")
  }

  do.test("Won games -> Finished game where X won via top row victory") {
    Expect.value(StateOfTicTacToe.gamestate([
      "XXX",
      "XOO",
      "O  "
    ])).toEqual("win")
  }

  do.test("Won games -> Finished game where X won via middle row victory") {
    Expect.value(StateOfTicTacToe.gamestate([
      "O  ",
      "XXX",
      " O "
    ])).toEqual("win")
  }

  do.test("Won games -> Finished game where X won via bottom row victory") {
    Expect.value(StateOfTicTacToe.gamestate([
      " OO",
      "O X",
      "XXX"
    ])).toEqual("win")
  }

  do.test("Won games -> Finished game where O won via top row victory") {
    Expect.value(StateOfTicTacToe.gamestate([
      "OOO",
      "XXO",
      "XX "
    ])).toEqual("win")
  }

  do.test("Won games -> Finished game where O won via middle row victory") {
    Expect.value(StateOfTicTacToe.gamestate([
      "XX ",
      "OOO",
      "X  "
    ])).toEqual("win")
  }

  do.test("Won games -> Finished game where O won via bottom row victory") {
    Expect.value(StateOfTicTacToe.gamestate([
      "XOX",
      " XX",
      "OOO"
    ])).toEqual("win")
  }

  do.test("Won games -> Finished game where X won via falling diagonal victory") {
    Expect.value(StateOfTicTacToe.gamestate([
      "XOO",
      " X ",
      "  X"
    ])).toEqual("win")
  }

  do.test("Won games -> Finished game where X won via rising diagonal victory") {
    Expect.value(StateOfTicTacToe.gamestate([
      "O X",
      "OX ",
      "X  "
    ])).toEqual("win")
  }

  do.test("Won games -> Finished game where O won via falling diagonal victory") {
    Expect.value(StateOfTicTacToe.gamestate([
      "OXX",
      "OOX",
      "X O"
    ])).toEqual("win")
  }

  do.test("Won games -> Finished game where O won via rising diagonal victory") {
    Expect.value(StateOfTicTacToe.gamestate([
      "  O",
      " OX",
      "OXX"
    ])).toEqual("win")
  }

  do.test("Won games -> Finished game where X won via a row and a column victory") {
    Expect.value(StateOfTicTacToe.gamestate([
      "XXX",
      "XOO",
      "XOO"
    ])).toEqual("win")
  }

  do.test("Won games -> Finished game where X won via two diagonal victories") {
    Expect.value(StateOfTicTacToe.gamestate([
      "XOX",
      "OXO",
      "XOX"
    ])).toEqual("win")
  }

  do.test("Drawn games -> Draw") {
    Expect.value(StateOfTicTacToe.gamestate([
      "XOX",
      "XXO",
      "OXO"
    ])).toEqual("draw")
  }

  do.test("Drawn games -> Another draw") {
    Expect.value(StateOfTicTacToe.gamestate([
      "XXO",
      "OXX",
      "XOO"
    ])).toEqual("draw")
  }

  do.test("Ongoing games -> Ongoing game: one move in") {
    Expect.value(StateOfTicTacToe.gamestate([
      "   ",
      "X  ",
      "   "
    ])).toEqual("ongoing")
  }

  do.test("Ongoing games -> Ongoing game: two moves in") {
    Expect.value(StateOfTicTacToe.gamestate([
      "O  ",
      " X ",
      "   "
    ])).toEqual("ongoing")
  }

  do.test("Ongoing games -> Ongoing game: five moves in") {
    Expect.value(StateOfTicTacToe.gamestate([
      "X  ",
      " XO",
      "OX "
    ])).toEqual("ongoing")
  }

  Expect.that {
    StateOfTicTacToe.gamestate([
      "XX ",
      "   ",
      "   "
    ])
  }.abortsWith("Wrong turn order: X went twice")

  Expect.that {
    StateOfTicTacToe.gamestate([
      "OOX",
      "   ",
      "   "
    ])
  }.abortsWith("Wrong turn order: O started")

  Expect.that {
    StateOfTicTacToe.gamestate([
      "XXX",
      "OOO",
      "   "
    ])
  }.abortsWith("Impossible board: game should have ended after the game was won")

  Expect.that {
    StateOfTicTacToe.gamestate([
      "XXX",
      "OOO",
      "XOX"
    ])
  }.abortsWith("Impossible board: game should have ended after the game was won")
}
