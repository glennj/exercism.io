I have 3 different implementations here:

1. all the values for each square are simply hardcoded
2. calling out to `bc` to perform the calculcations
    - optimized to minimize the number of `bc` invocations
3. performing the addition in bash, using string manipulation

Obviously #1 should be extremely fast, but how do the others
compare?

I was informed about a benchmarking tool called
[hyperfine](https://github.com/sharkdp/hyperfine). It's very
good but the command line interface is a but clunky. I've
wrapped it in a script (see `bencharking.sh` below). The
output is in `benchmarking.out`.

The results:

1. the hardcoded values script is the fastest. No surprise.
2. the bc script was less than 2x slower, which is a
   pretty good result, faster than I expected.
3. the bash-only script is 40x slower. Not unexpected. I
   did not code that for performance, only as a proof that,
   for the extremely stubborn, bash can do addition with
    arbitrarily large integers.

For the error case, where no looping is required, the
bash-only script was still 2x slower. I attribute this to
the sourcing of library files.
