import gleam/list

pub fn new_list() -> List(String) {
  []
}

pub fn existing_list() -> List(String) {
  ["Gleam", "Go", "TypeScript"]
}

pub fn add_language(languages: List(String), language: String) -> List(String) {
  [language, ..languages]
}

pub fn count_languages(languages: List(String)) -> Int {
  languages |> list.length
}

pub fn reverse_list(languages: List(String)) -> List(String) {
  languages |> list.reverse
}

pub fn exciting_list(languages: List(String)) -> Bool {
  case languages {
    ["Gleam", .._] | [_, "Gleam"] | [_, "Gleam", _] -> True
    _ -> False
  }
}
