import gleam/list
import gleam/result
import gleam/string

pub fn to_rna(dna: String) -> Result(String, Nil) {
  dna |> string.to_graphemes()
      |> list.try_map(transcribe)
      |> result.map(string.concat)
}

fn transcribe(nucleotide: String) -> Result(String, Nil) {
  case nucleotide {
    "G" -> Ok("C")
    "C" -> Ok("G")
    "T" -> Ok("A")
    "A" -> Ok("U")
    _ -> Error(Nil)
  }
}
