Iterating over a string by indexing each character can be slow
for big strings. 

Using a benchmarking tool called
[hyperfine](https://github.com/sharkdp/hyperfine): It's very
good but the command line interface is a but clunky. I've
wrapped it in a script (see `benchark.sh` below). 

For shorter strings, the overhead of setting up file descriptors
can override the cost of string indexing, but for larger strings
the benefit becomes clear.

```none
$ bash benchmark.sh
benchmark with length 127

Benchmark #1: whileLoop
  Time (mean ± σ):      19.4 ms ±   1.8 ms    [User: 8.2 ms, System: 8.6 ms]
  Range (min … max):    17.5 ms …  26.9 ms    74 runs

Benchmark #2: forLoop
  Time (mean ± σ):      11.9 ms ±   1.2 ms    [User: 4.9 ms, System: 2.8 ms]
  Range (min … max):     8.0 ms …  14.6 ms    132 runs

Summary
  'forLoop' ran
    1.64 ± 0.23 times faster than 'whileLoop'

benchmark with length 255

Benchmark #1: whileLoop
  Time (mean ± σ):      27.5 ms ±   1.1 ms    [User: 12.3 ms, System: 12.1 ms]
  Range (min … max):    26.2 ms …  34.5 ms    61 runs

Benchmark #2: forLoop
  Time (mean ± σ):      12.5 ms ±   1.7 ms    [User: 6.9 ms, System: 2.1 ms]
  Range (min … max):     9.6 ms …  15.4 ms    112 runs

Summary
  'forLoop' ran
    2.19 ± 0.31 times faster than 'whileLoop'

benchmark with length 4095

Benchmark #1: whileLoop
  Time (mean ± σ):     300.3 ms ±   7.0 ms    [User: 147.9 ms, System: 131.6 ms]
  Range (min … max):   290.5 ms … 309.5 ms    10 runs

Benchmark #2: forLoop
  Time (mean ± σ):     319.4 ms ±   8.4 ms    [User: 314.0 ms, System: 2.1 ms]
  Range (min … max):   312.1 ms … 339.1 ms    10 runs

Summary
  'whileLoop' ran
    1.06 ± 0.04 times faster than 'forLoop'

benchmark with length 32767

Benchmark #1: whileLoop
  Time (mean ± σ):      2.388 s ±  0.061 s    [User: 1.191 s, System: 1.041 s]
  Range (min … max):    2.284 s …  2.452 s    10 runs

Benchmark #2: forLoop
  Time (mean ± σ):     16.482 s ±  0.219 s    [User: 16.385 s, System: 0.067 s]
  Range (min … max):   16.200 s … 16.830 s    10 runs

Summary
  'whileLoop' ran
    6.90 ± 0.20 times faster than 'forLoop'
```
