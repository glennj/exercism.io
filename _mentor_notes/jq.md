# jq

## regular-chatbot

overall, very nice.

A couple of regex tips.

line 9: that won't match a line where "Chatbot" is the _only_ text on the line, because `\\W` needs to match a character. You can use the `\\b` directive which is a zero-width assertion: on one side is a word char and on the other side is not.

line 18: this is great. The requirements don't specify that case-insensitive is required, the they don't forbid it either.

line 49: using named groups and thae [`capture`](https://jqlang.github.io/jq/manual/v1.7/#capture) filter can simplify how you extract the captured text:
```sh
echo "My name is Glenn" \
| jq -R 'match("My name is ([-\\w]+)"; "i")'
```
```json
{
  "offset": 0,
  "length": 16,
  "string": "My name is Glenn",
  "captures": [
    {
      "offset": 11,
      "length": 5,
      "string": "Glenn",
      "name": null
    }
  ]
}
```
versus
```sh
echo "My name is Glenn" \
| jq -R 'capture("My name is (?<name>[-\\w]+)"; "i")'
```
```json
{
  "name": "Glenn"
}
```
