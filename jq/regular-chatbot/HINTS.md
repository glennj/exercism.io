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

[flags]: https://jqlang.org/manual/#regular-expressions
[regex-test]: https://jqlang.org/manual/#test
[regex-gsub]: https://jqlang.org/manual/#gsub
[regex-match]: https://jqlang.org/manual/#match
[regex-scan]: https://jqlang.org/manual/#scan
[regex-split]: https://jqlang.org/manual/#split-2
[regex-capture]: https://jqlang.org/manual/#capture
[named-capture]: https://riptutorial.com/regex/example/2479/named-capture-groups
[phone-validation]: https://www.w3resource.com/javascript/form/phone-no-validation.php