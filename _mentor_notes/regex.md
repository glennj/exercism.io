# Some of my postings about regexes

(after having given an example `\d{9}`)

The braces also allow for ranges:

* `.{5,10}` -- at least 5 and at most 10 characters
* `.{5,}` -- at least 5 characters, up to the rest of the string.
* `.{,10}` -- from zero up to 10 characters

<!-- -->

The braces is a "quantifier", similar to these:

* `.*` -- matches ZERO or more characters
* `.+` -- matches ONE or more characters
* `.?` -- matches zero or one character

The question mark is useful like this:

```dart
// the "s" is optional
var regex = RegExp(r'\d+ apples?');
var match;

match = regex.firstMatch("I have 10 apples.");
print(match[0]); // "10 apples"

match = regex.firstMatch("I have 1 apple.");
print(match[0]); // "1 apple" -- zero "s" matched

match = regex.firstMatch("I have no apples.");
print(match); // match is null, no digits in the string.
```

<!-- -->

These quantifiers are "greedy", they will try to capture as many as possible. The general rule for regex matching is "**leftmost longest**"

The regex `A+` will match the first 3 A's in "xxxAAAyyyAAAAAAz"

However, `A*` can match **zero** or more characters: will match _the zero-length string in front of the first "x"_. This sometimes catches people by surprise.
