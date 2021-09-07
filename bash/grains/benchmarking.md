I have 2 different implementations here:

1. all the values for each square are simply hardcoded
2. calling out to `bc` to perform the calculcations
    - optimized to minimize the number of `bc` invocations
3. performing the addition in bash, using string manipulation

Obviously #1 should be extremely fast, but how do the others
compare?

This test uses benchmarking tool [hyperfine](https://github.com/sharkdp/hyperfine).
It's very good but the command line interface is a bit clunky.
I've wrapped it in a script (see `bencharking.sh` below). The
output is in `benchmarking.out`.

The results:

1. the hardcoded values script is the fastest. No surprise.
2. the bc script was less than 33% slower, which is a
   pretty good result, faster than I expected.
3. the bash-only script is 10-20x slower. Not unexpected. I
   did not code that for performance, only as a proof that,
   for the extremely stubborn, bash can do addition with
    arbitrarily large integers.
