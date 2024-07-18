import gleam/string

pub fn score(word: String) -> Int {
  case string.pop_grapheme(word) {
    Error(Nil) -> 0
    Ok(#(letter, rest)) -> letter_score(letter) + score(rest)
  }
}

fn letter_score(letter: String) -> Int {
  case string.uppercase(letter) {
    "A"|"E"|"I"|"O"|"U"|"L"|"N"|"R"|"S"|"T" -> 1
    "D"|"G" -> 2
    "B"|"C"|"M"|"P" -> 3
    "F"|"H"|"V"|"W"|"Y" -> 4
    "K" -> 5
    "J"|"X" -> 8
    "Q"|"Z" -> 10
    _ -> 0
  }
}
