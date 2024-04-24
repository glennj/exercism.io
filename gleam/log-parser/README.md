# Log Parser

Welcome to Log Parser on Exercism's Gleam Track.
If you need help running the tests or submitting your code, check out `HELP.md`.
If you get stuck on the exercise, check out `HINTS.md`, but try and solve it without using those first :)

## Introduction

## Regular Expressions

Regular expressions in Gleam follow the **PCRE** specification (**P**erl **C**ompatible **R**egular **E**xpressions), similarly to other popular languages like Java, JavaScript, or Ruby.

The `gleam/regex` module offers functions for working with regular expressions.

~~~~exercism/note
This exercise assumes that you already know regular expression syntax, including character classes, quantifiers, groups, and captures.

if you need to refresh your regular expression knowledge, check out one of those sources: [Regular-Expressions.info](https://www.regular-expressions.info), [Rex Egg](https://www.rexegg.com/), [RegexOne](https://regexone.com/), [Regular Expressions 101](https://regex101.com/), [RegExr](https://regexr.com/).
~~~~

The most common way to create regular expressions is using the `regex.from_string` function.

```gleam
let assert Ok(re) = regex.from_string("test")
```

The regular expression creation functions return an error if the regular expression syntax is invalid, so a let-assertion has been used here to ensure the regular expression is valid.

The `regex.check` function can be used to check if a regular expression matches a string.

```gleam
let assert Ok(re) = regex.from_string("test")

regex.check(re, "this is a test")
// -> True

regex.check(re, "this is too")
// -> False
```


### Captures

If you wish to capture substrings using a regular expression the `regex.scan` function can be used to return a list of matches.

```gleam
let assert Ok(re) = regex.from_string("[oi]n a (\\w+)")
regex.scan(with: re, content: "I am on a boat in a lake.")
// -> [
//   Match(
//     content: "on a boat",
//     submatches: [Some("boat")]
//   ),
//   Match(
//     content: "in a lake",
//     submatches: [Some("lake")]
//   )
// ]
```

### Modifiers

The behaviour of a regular expression can be modified by creating it with the `regex.compile` function and passing in options.

```gleam
let options = regex.Options(case_insensitive: True, multi_line: False)
let assert Ok(re) = regex.compile("[A-Z]", with: options)
regex.check(re, "abc123")
// -> True
```

## Instructions

After a recent security review you have been asked to clean up the organization's archived log files.

## 1. Identify garbled log lines

You need some idea of how many log lines in your archive do not comply with current standards.
You believe that a simple test reveals whether a log line is valid.
To be considered valid a line should begin with one of the following strings:

- `[DEBUG]`
- `[INFO]`
- `[WARNING]`
- `[ERROR]`

Implement the `is_valid_line` function to return `True` if the log line is valid.

```gleam
log_parser.valid_line("[ERROR] Network Failure")
// -> True

log_parser.valid_line("Network Failure")
// -> False
```

## 2. Split the log line

Shortly after starting the log parsing project, you realize that one application's logs aren't split into lines like the others. In this project, what should have been separate lines, is instead on a single line, connected by fancy arrows such as `<--->` or `<*~*~>`.

In fact, any string that has a first character of `<`, a last character of `>`, and zero or more of the following characters `~`, `*`, `=`, and `-` in between can be used as a separator in this project's logs.

Implement the `split_line` function that takes a line and returns a list of strings.

```gleam
log_parser.split_line("[INFO] Start.<*>[INFO] Processing...<~~~>[INFO] Success.")
// -> ["[INFO] Start.", "[INFO] Processing...", "[INFO] Success."]
```

## 3. Tag lines with user names

You have noticed that some of the log lines include sentences that refer to users.
These sentences always contain the string `"User"`, followed by one or more whitespace characters, and then a user name.
You decide to tag such lines.

Implement a function `tag_with_user_name` that processes log lines:

- Lines that do not contain the string `"User"` remain unchanged.
- For lines that contain the string `"User"`, prefix the line with `[USER]` followed by the user name.

```gleam
log_parser.tag_with_user_name("[INFO] User Alice created a new project")
// -> "[USER] Alice [INFO] User Alice created a new project"
```

You can assume that:

- Each occurrence of the string `"User"` is followed by one or more whitespace character and the user name.
- There is at most one occurrence of the string `"User"` on each line.
- User names are non-empty strings that do not contain whitespace.

## Source

### Created by

- @lpil