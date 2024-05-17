import gleam/int

pub type Error {
  InvalidSquare
}

pub fn square(square: Int) -> Result(Int, Error) {
  case 1 <= square && square <= 64 {
    True -> Ok(int.bitwise_shift_left(1, square - 1))
    False -> Error(InvalidSquare)
  }
}

pub fn total() -> Int {
  int.bitwise_shift_left(1, 64) - 1
}
