Exemplary solution:
[https://exercism.io/mentor/solutions/97157571130f4749b0e2fb4948834bca][a]
[a]: https://exercism.io/mentor/solutions/97157571130f4749b0e2fb4948834bca

```bash
#!/usr/bin/env bash

(($1 % 2 != 0)) && exit

a=1

while true; do
    if (($1 * ($1 - 2 * a) % (2 * ($1 - a)) == 0)); then
        ((b = $1 * ($1 - 2 * a) / (2 * ($1 - a))))
        ((a >= b)) && break
        result+="$a,$b,$(($1 - a - b))\n"
    fi
    ((a++))
done

echo -en "$result"
```
