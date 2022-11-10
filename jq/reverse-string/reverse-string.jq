.value
| (. / "") as $chars             # `/` on a string is like `split/1`
| [range(length; 0; -1)]
| map($chars[. - 1])
| join("")

