import gleam/string
import gleam/list
import gleam/result
import simplifile as sf

pub fn read_emails(path: String) -> Result(List(String), Nil) {
  sf.read(path)
  |> result.map(fn(contents) {contents |> string.trim() |> string.split("\n")})
  |> result.nil_error()
}

pub fn create_log_file(path: String) -> Result(Nil, Nil) {
  sf.create_file(path)
  |> result.nil_error()
}

pub fn log_sent_email(path: String, email: String) -> Result(Nil, Nil) {
  sf.append(path, email <> "\n")
  |> result.nil_error()
}

pub fn send_newsletter(
  emails_path: String,
  log_path: String,
  send_email: fn(String) -> Result(Nil, Nil),
) -> Result(Nil, Nil) {
  read_emails(emails_path)
  |> result.try(fn(emails) {
      list.try_each(emails, fn(email) {
        send_email(email)
        |> result.map(fn(_x) { log_sent_email(log_path, email) })
        |> result.nil_error()
      })
    })
}
