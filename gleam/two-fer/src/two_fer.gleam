import gleam/option.{type Option}

pub fn two_fer(name: Option(String)) -> String {
  let id = option.unwrap(name, "you")
  "One for " <> id <> ", one for me."
}
