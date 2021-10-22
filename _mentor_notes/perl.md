# Intro

Hey, nice job completing the exercise.

To start with, as a mentor, perl is kind of hard to give advice for (there's more than one way ...). But, some thoughts:

## all-your-base

Line 45, don't need to `reverse` if you use `unshift` on lines 32 and 41.

<details><summary>Lines 23-28 can be expressed as a one-liner with a <code>reduce</code> function. Click for more details.</summary>

---

As we iterate over the digits, we accumulate the result by multiplying the current accumulation by the input base and adding the next digit.
The [`reduce` function from List::Util](https://perldoc.pl/List::Util#reduce) is handy here.

<details><summary>Click for a spoiler...</summary>

```perl
use List::Util qw(reduce);
@binary_digits = (1,0,0,0,0,0);
$base = 2;
my $result = reduce {$a * $base + $b} @binary_digits # => 32
```
</details>

This implies that validating the input digits being in the right range is
done separately. The [`any` or `all` List::Util
functions](https://perldoc.pl/List::Util#any) help:

<details><summary>Spoiler...</summary>

```perl
die "all digits must satisfy..."
    unless all {0 <= ... < $base} @digits;
```
</details>
</details>

