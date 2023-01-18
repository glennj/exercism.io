# Hints

## 1. Check Valid Command

- Use the [test][regex-test] expression for returning a boolean.
- Remember that we can use the [flags][flags] argument for additional features such as case insensitivity.

## 2. Remove Encrypted Emojis

- Thanks to the common encryption of each emoji, we can use `[0-9]` to search for any digit after the `emoji` word.
- The character `+` matches one or more consecutive items.
- Use the [gsub][regex-gsub] method to replace each emoji with an empty string.

## 3. Check Valid Phone Number

- This [article][phone-validation] is really good at explaining different ways to validate a phone number.
- Use the `test` filter to check whether the phone number is valid or not.
- Return the final answer with an `if-then-else` expression.

## 4. Get Website Link

- We are targeting words that are joined by one or more `.` dots.
- The [match][regex-match] filter with the `g` flag can be used.
- The [scan][regex-scan] filter might be easier to use.

## 5. Greet the User

- Using [named capture groups][named-capture] is convenient to re-use the matched text.
- The [capture][regex-capture] filter is nice for referring to the named group.

## 6. Very Simple CSV Parsing

- Here, we want to use [split][regex-split] because we know the delimiter.
- The pattern is "comma followed by zero or more whitespace".
- We have to use the **2-arity** `split` filter; there are no flags that need to be specified.

[flags]: https://stedolan.github.io/jq/manual/v1.6/#RegularexpressionsPCRE
[regex-test]: https://stedolan.github.io/jq/manual/v1.6/#test(val),test(regex;flags)
[regex-gsub]: https://stedolan.github.io/jq/manual/v1.6/#gsub(regex;string),gsub(regex;string;flags)
[regex-match]: https://stedolan.github.io/jq/manual/v1.6/#match(val),match(regex;flags)
[regex-scan]: https://stedolan.github.io/jq/manual/v1.6/#scan(regex),scan(regex;flags)
[regex-split]: https://stedolan.github.io/jq/manual/v1.6/#split(regex;flags)
[regex-capture]: https://stedolan.github.io/jq/manual/v1.6/#capture(val),capture(regex;flags)
[named-capture]: https://riptutorial.com/regex/example/2479/named-capture-groups
[phone-validation]: https://www.w3resource.com/javascript/form/phone-no-validation.php