import gleam/result

pub type Error {
  InvalidBase(Int)
  InvalidDigit(Int)
}

pub fn rebase(
  digits digits: List(Int),
  input_base input_base: Int,
  output_base output_base: Int,
) -> Result(List(Int), Error) {
  to_decimal(digits, input_base, 0)
  |> result.try(to_output_base(_, output_base, []))
}

fn to_decimal(digits, ibase, acc) -> Result(Int, Error) {
  case ibase, digits {
    b, _ if b < 2 -> Error(InvalidBase(b))
    _, [] -> Ok(acc)
    _, [digit, ..rest] -> 
      case digit {
        d if d < 0 || d >= ibase -> Error(InvalidDigit(d))
        _ -> to_decimal(rest, ibase, acc * ibase + digit)
      }
  }
}

fn to_output_base(decimal, obase, acc) -> Result(List(Int), Error) {
  case obase, decimal, acc {
    b, _, _ if b < 2 -> Error(InvalidBase(b))
    _, 0, [] -> Ok([0])
    _, 0, _ -> Ok(acc)
    _, _, _ -> to_output_base(decimal / obase, obase, [decimal % obase, ..acc])
  }
}
