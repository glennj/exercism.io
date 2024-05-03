import gleam/string
import gleam/list

type Key {
  Key(lower: String, graphemes: List(String))
}

pub fn find_anagrams(word: String, candidates: List(String)) -> List(String) {
  let subject = key(word)
  list.filter(candidates, is_anagram(subject, _))
}

fn is_anagram(subject: Key, candidate: String) -> Bool {
  let cand = key(candidate)
  subject.lower != cand.lower && subject.graphemes == cand.graphemes
}

fn key(word: String) -> Key {
  let lc = string.lowercase(word)
  Key(lower: lc, graphemes: lc |> string.to_graphemes() |> list.sort(string.compare))
}
