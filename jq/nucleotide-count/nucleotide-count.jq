include "lib/assert";

{A: 0, C: 0, G: 0, T: 0} as $count

| .strand / ""
| assert(all(in($count)) ; "Invalid nucleotide in strand")
| reduce .[] as $nucleotide ($count; .[$nucleotide] += 1)
