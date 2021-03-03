Exemplary solution: 
[https://exercism.io/mentor/solutions/7274e7c161b04100962835bae63ea518][a]
[a]: https://exercism.io/mentor/solutions/7274e7c161b04100962835bae63ea518


```bash
#!/usr/bin/env bash

die() {
    echo "$1" 1>&2
    exit 1
}

for frame in {1..10}; do
    value=0

    for throw in {1..3}; do
        if ((throw < 3 || fill)); then
            [[ ! $1 ]] && die 'Score cannot be taken until the end of the game'
            (($1 < 0)) && die 'Negative roll is invalid'
            ((value += $1))
            ((value > 10)) && die 'Pin count exceeds pins on the lane'
            shift
        fi

        if ((value == 10)); then
            if ((frame == 10)); then
                fill=1
                ((score += value))
                value=0
            else
                ((value += $1))
                if ((throw == 1)); then
                    ((value += $2))
                    break
                fi
            fi
        fi
    done

    ((score += value))
done

[[ $1 ]] && die 'Cannot roll after game is over'

echo "$score"
```
