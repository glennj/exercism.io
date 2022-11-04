include "lib/assert";

assert( (.strand1 | length) == (.strand2 | length); "strands must be of equal length")
| [(.strand1 | explode), (.strand2 | explode)]
| transpose
| map(if first != last then 1 else 0 end)
| add + 0
