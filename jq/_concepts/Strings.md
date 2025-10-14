# About

Recall that `jq` supports the same datatypes as JSON.
JSON describes **strings** as:

> A string is a sequence of zero or more Unicode characters, wrapped in double quotes, using backslash escapes.

## Escape sequences

Within a string, use the backslash character to embed "special" characters:

- `\"` a literal quotation mark,
- `\\` a literal backslash,
- whitespace characters `\n`, `\t`, `\r`, `\f`, `\b`
- unicode characters `\uAAAA` where `A` is a hex digit

## String length

To find the number of characters, use the [`length`][length] function.

```sh
$ jq -cn --args  '$ARGS.positional[] | length' "Hello world!" "❄🌡🤧🤒🏥🕰😀"
12
7
```

To find the number of actual _bytes_, use the [`utf8bytelength`][utf8bytelength] function.

```sh
$ jq -cn --args  '$ARGS.positional[] | utf8bytelength' "Hello world!" "❄🌡🤧🤒🏥🕰😀"
12
27
```

## Substrings

The [**slice** notation][slice] can be used to extract substrings.
The syntax is `.[i:j]`.
The substring returned is of length `j - i`, returning characters from index `i` (inclusive) to index `j` (exclusive).
Either index can be negative, in which case it counts backwards from the end of the string.
Either index can be omitted, in which case it refers to the start or end of the string.
Indexes are zero-based.

```jq
"abcdefghij"[3:6]   # => "def"
"abcdefghij"[3:]    # => "defghij"
"abcdefghij"[:-2]   # => "abcdefgh"
```

## Concatenation

Use [`+`][+] to join strings:

```jq
"Hello" + " " + "world!"
# => "Hello world!"
```

Use [`add`][add] when given an array of strings:

```jq
["Hello", " ", "world!"] | add
# => "Hello world!"
```

## Split and Join

Splitting uses the [`split/1`][split/1] function or the [`/`][/] operator:

```jq
"Hello beautiful world!" | split(" ")   # => ["Hello", "beautiful", "world!"]
```

```jq
"Hello beautiful world!" / " "          # => ["Hello", "beautiful", "world!"]
```

The inverse of [`split/1`][split/1] is [`join/1`][join/1].

Converting a string into an array of characters is frequently needed.
There are two ways to do this:

1. split on an empty string

   ```jq
   "Hi friend 😀" / ""   # => ["H","i"," ","f","r","i","e","n","d"," ","😀"]
   ```

1. [`explode`][explode] into an array of _codepoints_

   ```jq
   "Hi friend 😀" | explode   # => [72,105,32,102,114,105,101,110,100,32,128512]
   ```

   [`implode`][implode] is the inverse of [`explode`][explode].

## Find the index of a substring

Use the [`index/1`][index/1] function; the index is zero-based.

```jq
"hello" | index("el")'   # => 1
```

If the substring is not in the string, the result is `null`

```jq
"hello" | index("elk")   # => null
```

Other useful functions are [`rindex/1`][index/1] and [`indices`][indices].

## String interpolation

Within a string, the sequence `\(expression)` will embed the _result_ of that expression [into the string][interpolate]:

```jq
"The current datetime is \(now | strflocaltime("%c"))"   # => "The current datetime is Wed Nov 16 17:06:33 2022"
```

## Case conversion

Use the [`ascii_downcase`][ascii_downcase] and [`ascii_upcase`][ascii_downcase] functions to change the case of a string.

## Cast a string to a number

Use the [`tonumber`][tonumber] function to create a number from a string.

This raises an error if the string cannot be cast to a number, so [`try-catch`][try-catch] may be needed.

```jq
$ jq -cn --args '$ARGS.positional[] | [., tonumber]' 42 3.14 oops 1e6 0xbeef
["42",42]
["3.14",3.14]
jq: error (at <unknown>): Invalid numeric literal at EOF at line 1, column 4 (while parsing 'oops')

$ echo $?
5

$ jq -cn --args '$ARGS.positional[] | try [., tonumber] catch "not a number"' 42 3.14 oops 1e6 0xbeef
["42",42]
["3.14",3.14]
"not a number"
["1e6",1000000]
"not a number"
```

Note that [JSON numbers][json-numbers] do not include hex or octal variations that some languages support.

## Other functions

Check [the manual][manual] for more details about these functions:

- repeat a string: `*`
- testing containment: `contains/1`, `inside/1`, `indices/1`, `index/1`, `rindex/1`, `startswith/1`, `endswith/1`
- trim: `lsrimstr/1`, `rtrimstr/1`
- formatting functions: `@text`, `@json`, `@html`, `@uri`, `@csv`, `@tsv`, `@sh`, `@base64`, `@base64d`

## Regular expressions

`jq` has rich support for regular expressions: this will be the topic of a later lesson.

[manual]: https://jqlang.github.io/jq/manual/v1.7/
[interpolate]: https://jqlang.github.io/jq/manual/v1.7/#string-interpolation
[length]: https://jqlang.github.io/jq/manual/v1.7/#length
[utf8bytelength]: https://jqlang.github.io/jq/manual/v1.7/#utf8bytelength
[+]: https://jqlang.github.io/jq/manual/v1.7/#addition
[/]: https://jqlang.github.io/jq/manual/v1.7/#multiplication-division-modulo
[add]: https://jqlang.github.io/jq/manual/v1.7/#add
[split/1]: https://jqlang.github.io/jq/manual/v1.7/#split-1
[join/1]: https://jqlang.github.io/jq/manual/v1.7/#join
[explode]: https://jqlang.github.io/jq/manual/v1.7/#explode
[implode]: https://jqlang.github.io/jq/manual/v1.7/#implode
[ascii_downcase]: https://jqlang.github.io/jq/manual/v1.7/#ascii_downcase-ascii_upcase
[tonumber]: https://jqlang.github.io/jq/manual/v1.7/#tonumber
[try-catch]: https://jqlang.github.io/jq/manual/v1.7/#try-catch
[json-numbers]: https://www.json.org/json-en.html
[indices]: https://jqlang.github.io/jq/manual/v1.7/#indices
[index/1]: https://jqlang.github.io/jq/manual/v1.7/#index-rindex
[slice]: https://jqlang.github.io/jq/manual/v1.7/#array-string-slice
