# About

**Regular expressions** (regexes) are sequences of characters that specify a search pattern in text.

Learning regular expression syntax is beyond the scope of this topic.
We will focus on the expressions that `jq` provides to utilize regexes.

## Regex Flavour

Different tools implement different versions of regular expressions.
`jq` incorporates the [Oniguruma][oniguruma] regex library that is largely compatible with Perl v5.8 regexes.

The specific syntax used by `jq` version 1.7 can be [found on the Oniguruma GitHub repo][onig-syntax].

<!-- prettier-ignore -->
~~~~exercism/caution
`jq` does not have any special syntax for regular expressions.
They are simply expressed as strings.
That means that any backslashes in the regular expression need to be escaped in the string.

For example, the digit character class (`\d`) must be written as `"\\d"`.
~~~~

<!-- prettier-ignore-end -->

## Regex Functions

Regular expressions in `jq` are limited to [a set of filters][jq-regex-funcs].

### Simple Matching

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

### Information About the Match

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

### Captured Substrings

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

### Just the Substrings

The `scan` filter is similar to `match` with the `"g"` flag.

```jq
STRING | scan(REGEX)
STRING | scan(REGEX; FLAGS)

# note, there is no scan([REGEX, FLAGS]) version, unlike other filters
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

~~~~exercism/note
Note that jq v1.6 does _not_ implement the 2-argument `scan` function, even though the version 1.6 manual [says it does][manual-scan-1.6]:

* [version 1.7 source code][src-scan-1.7]
* [version 1.6 source code][src-scan-1.6]

[manual-scan-1.6]: https://jqlang.github.io/jq/manual/v1.6/#scan
[src-scan-1.7]: https://github.com/jqlang/jq/blob/11c528d04d76c9b9553781aa76b073e4f40da008/src/builtin.jq#L92
[src-scan-1.6]: https://github.com/jqlang/jq/blob/2e01ff1fb69609540b2bdc4e62a60499f2b2fb8e/src/builtin.jq#L90
~~~~

### Splitting a String

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

<!-- prettier-ignore -->
~~~~exercism/note
This is what happens if we forget the flags argument.

```jq
"first   second           third fourth" | split("\\s+")
# => ["first   second           third fourth"]
```
Only one result: the _fixed string_ `\s+` was not seen in the input.

The 1-arity `split` cannot handle _arbitrary_ whitespace.
Splitting on a space gives this result.

```jq
"first   second           third fourth" | split(" ")
# => ["first", "", "", "second", "", "", "", "",
#     "", "", "", "", "", "", "third", "fourth" ]
```
~~~~

<!-- prettier-ignore-end -->

### Substitutions

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

## Flags

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
[onig-syntax]: https://github.com/kkos/oniguruma/blob/v6.9.9/doc/RE
[jq-regex-funcs]: https://jqlang.github.io/jq/manual/v1.7/#regular-expressions
[jq-interp]: https://jqlang.github.io/jq/manual/v1.7/#string-interpolation
