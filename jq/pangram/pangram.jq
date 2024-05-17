.sentence
| ascii_upcase / ""
| map(select("A" <= . and . <= "Z"))
| unique
| join("") == "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
