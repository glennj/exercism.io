import "./diamond" for Diamond
import "wren-testie/testie" for Testie, Expect

Testie.test("Diamond") { |do, skip|
  do.test("Degenerate case with a single 'A' row") {
    Expect.that(Diamond.rows("A")).toEqual(["A"])
  }

  skip.test("Degenerate case with no row containing 3 distinct groups of spaces") {
    // prettier-ignore
    Expect.that(Diamond.rows("B")).toEqual([
      " A ",
      "B B",
      " A "
    ])
  }

  skip.test("Smallest non-degenerate case with odd diamond side length") {
    // prettier-ignore
    Expect.that(Diamond.rows("C")).toEqual([
      "  A  ",
      " B B ",
      "C   C",
      " B B ",
      "  A  "
    ])
  }

  skip.test("Smallest non-degenerate case with even diamond side length") {
    Expect.that(Diamond.rows("D")).toEqual([
      "   A   ",
      "  B B  ",
      " C   C ",
      "D     D",
      " C   C ",
      "  B B  ",
      "   A   ",
    ])
  }

  skip.test("Largest possible diamond") {
    Expect.that(Diamond.rows("Z")).toEqual([
      "                         A                         ",
      "                        B B                        ",
      "                       C   C                       ",
      "                      D     D                      ",
      "                     E       E                     ",
      "                    F         F                    ",
      "                   G           G                   ",
      "                  H             H                  ",
      "                 I               I                 ",
      "                J                 J                ",
      "               K                   K               ",
      "              L                     L              ",
      "             M                       M             ",
      "            N                         N            ",
      "           O                           O           ",
      "          P                             P          ",
      "         Q                               Q         ",
      "        R                                 R        ",
      "       S                                   S       ",
      "      T                                     T      ",
      "     U                                       U     ",
      "    V                                         V    ",
      "   W                                           W   ",
      "  X                                             X  ",
      " Y                                               Y ",
      "Z                                                 Z",
      " Y                                               Y ",
      "  X                                             X  ",
      "   W                                           W   ",
      "    V                                         V    ",
      "     U                                       U     ",
      "      T                                     T      ",
      "       S                                   S       ",
      "        R                                 R        ",
      "         Q                               Q         ",
      "          P                             P          ",
      "           O                           O           ",
      "            N                         N            ",
      "             M                       M             ",
      "              L                     L              ",
      "               K                   K               ",
      "                J                 J                ",
      "                 I               I                 ",
      "                  H             H                  ",
      "                   G           G                   ",
      "                    F         F                    ",
      "                     E       E                     ",
      "                      D     D                      ",
      "                       C   C                       ",
      "                        B B                        ",
      "                         A                         ",
    ])
  }
}
