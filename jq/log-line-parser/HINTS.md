# Hints

## 1. Get message from a log line

- Extract the text following the _first_ colon.
  - You can use `index/1` and the slice notation.
  - Or explore using regular expression functions.
- Use the provided `trim` function.

## 2. Get log level from a log line

- Extract the text within the first set of brackets.
- Output the result in lower case.

## 3. Reformat a log line

- Reuse the existing functions.
- Two approaches are to use string interpolation or concatenation.