import gleam/int

pub fn convert(n: Int) -> String {
  let check = fn(sounds: String, div: Int, drop: String) -> String {
    sounds <> case n % div == 0 {
      True -> drop
      False -> ""
    }
  }

  let drops = "" |> check(3, "Pling") |> check(5, "Plang") |> check(7, "Plong")

  case drops {
    "" -> int.to_string(n)
    _ -> drops
  }
}
