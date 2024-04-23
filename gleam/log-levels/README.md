# Log Levels

Welcome to Log Levels on Exercism's Gleam Track.
If you need help running the tests or submitting your code, check out `HELP.md`.
If you get stuck on the exercise, check out `HINTS.md`, but try and solve it without using those first :)

## Introduction

## Strings

Strings in Gleam are immutable text surrounded by double quotes. They support unicode characters, and can be multi-line.

```gleam
let greeting = "Hello, Joe! 📞"

let multi_line_string = "one
two
three"
```

Strings can be joined together using the `<>` operator:

```gleam
let name = "Mike"
"Hello, " <> name <> "!"
// -> "Hello, Mike!"
```

The [`gleam/string`][stdlib] module in the standard library provides functions for working with strings.

Strings can be matched upon in case expressions:

```gleam
pub fn on_an_excellent_adventure(name: String) -> Bool {
  case name {
    "Bill" -> True
    "Ted" -> True
    _ -> False
  }
}
```

If you want to match on the beginning of a string and assign the rest to a variable you can use the `<>` operator pattern:

```gleam
pub fn profession(name: String) -> String {
  case name {
    "Dr " <> rest -> rest <> " is a doctor"
    _ -> "I'm not sure what " <> name <> " does"
  }
}
```

[stdlib]: https://hexdocs.pm/gleam_stdlib/gleam/string.html

## Instructions

In this exercise you'll be processing log-lines.

Each log line is a string formatted as follows: `"[<LEVEL>]: <MESSAGE>"`.

There are three different log levels:

- `INFO`
- `WARNING`
- `ERROR`

You have three tasks, each of which will take a log line and ask you to do something with it.

## 1. Get message from a log line

Implement the `message` function to return a log line's message:

```gleam
message("[ERROR]: Invalid operation")
// -> "Invalid operation"
```

Any leading or trailing white space should be removed:

```gleam
message("[WARNING]:  Disk almost full\r\n")
// -> "Disk almost full"
```

## 2. Get log level from a log line

Implement the `log_level` function to return a log line's log level, which should be returned in lowercase:

```gleam
log_level("[ERROR]: Invalid operation")
// -> "error"
```

## 3. Reformat a log line

Implement the `reformat` function that reformats the log line, putting the message first and the log level after it in parentheses:

```gleam
reformat("[INFO]: Operation completed")
// -> "Operation completed (info)"
```

## Source

### Created by

- @lpil