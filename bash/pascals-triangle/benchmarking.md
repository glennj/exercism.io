I had expected the factorial approach to be much slower.

```none
$ bash pascals_triangle.sh -B
Benchmark #1: 2dArray
  Time (mean ± σ):      23.3 ms ±   1.3 ms    [User: 15.0 ms, System: 3.2 ms]
  Range (min … max):    20.2 ms …  26.1 ms    100 runs

Benchmark #2: factorials
  Time (mean ± σ):      30.9 ms ±   2.1 ms    [User: 23.9 ms, System: 2.7 ms]
  Range (min … max):    27.0 ms …  36.3 ms    100 runs

Summary
  '2dArray' ran
    1.33 ± 0.12 times faster than 'factorials'
```
