# Regular Chatbot

Welcome to Regular Chatbot on Exercism's jq Track.
If you need help running the tests or submitting your code, check out `HELP.md`.
If you get stuck on the exercise, check out `HINTS.md`, but try and solve it without using those first :)

## Introduction

## Regular Expressions

**Regular expressions** (regexes) are sequences of characters that specify a search pattern in text.

Learning regular expression syntax is beyond the scope of this topic.
We will focus on the expressions that `jq` provides to utilize regexes.

### Regex Flavour

Different tools implement different versions of regular expressions.
`jq` incorporates the [Oniguruma][oniguruma] regex library that is largely compatible with Perl v5.8 regexes.

The specific syntax used by `jq` version 1.6 can be [found on the Oniguruma GitHub repo][onig-syntax].

<!-- prettier-ignore -->
~~~~exercism/caution
`jq` does not have any special syntax for regular expressions.
They are simply expressed as strings.
That means that any backslashes in the regular expression need to be escaped in the string.

For example, the digit character class (`\d`) must be written as `"\\d"`.
~~~~

<!-- prettier-ignore-end -->

### Regex Functions

Regular expressions in `jq` are limited to [a set of filters][jq-regex-funcs].

#### Simple Matching

When you need to know if a string matches a pattern, use the `test` filter.

```jq
STRING | test(REGEX)
STRING | test(REGEX; FLAGS)
STRING | test([REGEX, FLAGS])
```

This filter outputs a _boolean_ result.

```jq
"Hello World!" | test("W")    # => true
"Goodbye Mars" | test("W")    # => false
```

#### Information About the Match

When you need to extract the substring that actually matched the pattern, use the `match` filter.

```jq
STRING | match(REGEX)
STRING | match(REGEX; FLAGS)
STRING | match([REGEX, FLAGS])
```

This filter outputs:

- nothing if there was no match, or
- an object containing various properties if there was a match.

This example looks for two identical consecutive vowels by using the backref syntax, `\1`.

```jq
"Hello World!" | match("([aeiou])\\1")
# => empty

"Goodbye Mars" | match("([aeiou])\\1")
# => {
#      "offset": 1,
#      "length": 2,
#      "string": "oo",
#      "captures": [
#        {
#          "offset": 1,
#          "length": 1,
#          "string": "o",
#          "name": null
#        }
#      ]
#    }
```

The `match` filter returns an object for _each_ match.
This example shows the `"g"` flag in action to find all the vowels.

```jq
"Goodbye Mars" | match("[aeiou]"; "g")
# => { "offset": 1, "length": 1, "string": "o", "captures": [] }
#    { "offset": 2, "length": 1, "string": "o", "captures": [] }
#    { "offset": 6, "length": 1, "string": "e", "captures": [] }
#    { "offset": 9, "length": 1, "string": "a", "captures": [] }
```

#### Captured Substrings

Similar to the `match` filter, the `capture` filter returns an object if there was a match.

```jq
STRING | capture(REGEX)
STRING | capture(REGEX; FLAGS)
STRING | capture([REGEX, FLAGS])
```

The returned object is a mapping of the **named captures**.

```jq
"JIRAISSUE-1234" | capture("(?<project>\\w+)-(?<issue_num>\\d+)")
# => {
#      "project": "JIRAISSUE",
#      "issue_num": "1234"
#    }
```

#### Just the Substrings

The `scan` filter is similar to `match` with the `"g"` flag.

```jq
STRING | scan(REGEX)
STRING | scan(REGEX; FLAGS)
STRING | scan([REGEX, FLAGS])
```

`scan` will output a _stream_ of substrings.

```jq
"Goodbye Mars" | scan("[aeiou]")
# => "o"
#    "o"
#    "e"
#    "a"
```

Use the `[...]` array constructor to capture the substrings.

```jq
"Goodbye Mars" | [ scan("[aeiou]") ]
# => ["o", "o", "e", "a"]
```

#### Splitting a String

If you know the parts of the string you want to **keep**, use `match` or `scan`.
If you know the parts that you want to **discard**, use `split`.

```jq
STRING | split(REGEX; FLAGS)
```

<!-- prettier-ignore -->
~~~~exercism/caution
The **1-arity** `split` filter treats its argument as a **fixed string**.

To use a regex with `split`, you **must** provide the 2nd argument; it's OK to use an empty string.
~~~~

<!-- prettier-ignore-end -->

An example that splits a string on arbitrary whitespace.

```jq
"first   second           third fourth" | split("\\s+"; "")
# => ["first", "second", "third", "fourth"]
```

#### Substitutions

The `sub` and `gsub` filters can transform the input string, replacing matched portions of the input with a replacement string.

To replace just the first match, use `sub`.
To replace all the matches, use `gsub`.

```jq
STRING | sub(REGEX; REPLACEMENT)
STRING | sub(REGEX; REPLACEMENT; FLAGS)
STRING | gsub(REGEX; REPLACEMENT)
STRING | gsub(REGEX; REPLACEMENT; FLAGS)
```

```jq
"Goodnight kittens. Goodnight mittens." | sub("night"; " morning")
# => "Good morning kittens. Goodnight mittens."

"Goodnight kittens. Goodnight mittens." | gsub("night"; " morning")
# => "Good morning kittens. Good morning mittens."
```

The replacement text can refer to the matched substrings; use named captures and [string interpolation][jq-interp].

```jq
"Some 3-letter acronyms: gnu, csv, png"
| gsub( "\\b(?<tla>[[:alpha:]]{3})\\b";     # find words 3 letters long
        "\(.tla | ascii_upcase)" )          # upper-case the match
# => "Some 3-letter acronyms: GNU, CSV, PNG"
```

### Flags

In all the above filters, FLAGS is a string consisting of zero of more of the supported flags.

- `g` - Global search (find all matches, not just the first)
- `i` - Case insensitive search
- `m` - Multi line mode ('.' will match newlines)
- `n` - Ignore empty matches
- `p` - Both s and m modes are enabled
- `s` - Single line mode ('^' -> '\A', '$' -> '\Z')
- `l` - Find longest possible matches
- `x` - Extended regex format (ignore whitespace and comments)

For example

```jq
"JIRAISSUE-1234" | capture("(?<project>\\w+)-(?<issue_num>\\d+)")

# or with Extended formatting

"JIRAISSUE-1234" | capture("
                     (?<project>   \\w+ )  # the Jira project
                     -                     # followed by a hyphen
                     (?<issue_num> \\d+ )  # followed by digits
                   "; "x")
```

[oniguruma]: https://github.com/kkos/oniguruma
[onig-syntax]: https://github.com/kkos/oniguruma/blob/6fa38f4084b448592888ed9ee43c6e90a46b5f5c/doc/RE
[jq-regex-funcs]: https://stedolan.github.io/jq/manual/v1.6/#RegularexpressionsPCRE
[jq-interp]: https://stedolan.github.io/jq/manual/v1.6/#Stringinterpolation-%5C(foo)

## Instructions

You have been hired as a Regular Expression Specialist in a company that is developing a Chatbot.

It is in a very basic phase of development.
Your mission is to use Regular Expressions to improve the Chatbot's ability to understand and generate natural language.

## Check Valid Command

Apart from being smart, the Chatbot is also a loyal assistant.
To ask the Chatbot something, the user must say the word "**Chatbot**" in the first position of the command.
It doesn't matter if the keyword is in UPPERCASE or lowercase.
The important part is the position of the word.

Implement the function `is_valid_command` that helps the Chatbot recognize when the user is giving a command.

```jq
"Chatbot, play a song from the 80's." | is_valid_command
# => true
"Hey Chatbot, where is the closest pharmacy?" | is_valid_command
# => false
"CHATBOT, do you have a solution for this challenge?" | is_valid_command
# => true
```

## Remove Encrypted Emojis

The Chatbot has a difficult time understanding how humans use emojis to express their emotions.
When the Chatbot receives user messages, each emoji is represented as the string "emoji" followed by an _id number_.

Implement the `remove_emoji` method which takes a string and removes all the emoji throughout the message.

Lines not containing emojis should be returned unmodified.
Just remove the emoji string.
Do not adjust the whitespace.

```jq
"I love playing videogames emoji3465 it's one of my hobbies" | remove_emoji
# => "I love playing videogames  it's one of my hobbies"
```

## Check Valid Phone Number

At some point in the interaction with the Chatbot, the user will provide a phone number.
The Chatbot can only call a number with a specific format.

Implement the `check_phone_number` function.

If the number is valid, the Chatbot answers with a message thanking the user and confirming the number.
If the number is invalid, the Chatbot informs the user that the phone number is not valid.

The expected format is `(+NN) NNN-NNN-NNN`, where N is a digit.

```jq
"chatbot my phone number is (+34) 659-771-594" | check_phone_number
# => "Thanks! Your phone number is OK."
"chatbot, call me at 659-771-594" | check_phone_number
# => "Oops, it seems like I can't reach out to 659-771-594."
```

## Get Website Link

The Chatbot is a really curious software.
Even though it can search the internet for a particular topic, it likes to ask users about cool websites to visit to find relevant information.

Example conversation:

> **Chatbot**: Hey User, I would like to learn how to code in JavaScript. Do you know any cool website where I could learn?

> **User**: I learned a lot from exercism.org, there's lots of great stuff there.

Implement the function `get_domains` which returns an array of website domains.

```jq
"I learned a lot from exercism.org and google.com" | get_domains
# => ["exercism.org", "google.com"]
```

## Greet the User

A polite Chatbot will speak to users by name.
When a user introduces themselves, our Chatbot will detect their name and respond with a friendly greeting.

Write the function `nice_to_meet_you`.

If the input string contains "My name is Someone.", capture the name and return the string "Nice to meet you, Someone.".

```jq
"My name is Jean-Luc" | nice_to_meet_you
# => "Nice to meet you, Jean-Luc"
```

## Very Simple CSV Parsing

Yielding to "creeping featuritis", we'll add a CSV parsing function to the Chatbot.

Implement the `parse_csv` function that takes a string and returns an array of the resulting fields.
The field separator should be "comma plus optional whitespace".

We won't worry about any of the edge cases with the CSV format (such as fields containing commas).

```jq
"first, second,third,   fourth" | parse_csv
# => ["first", "second", "third", "fourth"]
```

## Source

### Created by

- @glennj