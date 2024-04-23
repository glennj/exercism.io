import gleam/string

pub fn message(log_line: String) -> String {
  case string.split_once(log_line, on: "]: ") {
    Ok(#(_, msg)) -> msg |> string.trim
    _ -> ""
  }
}

pub fn log_level(log_line: String) -> String {
  case string.split_once(log_line, on: "]: ") {
    Ok(#(level, _)) -> level |> string.drop_left(1) |> string.lowercase
    _ -> ""
  }
}

pub fn reformat(log_line: String) -> String {
  message(log_line) <> " (" <> log_level(log_line) <> ")"
}
