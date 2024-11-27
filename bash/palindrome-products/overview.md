# Palindrome Products results

This solution works locally but fails online in Exercism: it's just too slow.

It takes over a minute to run: adding timing info to the test run

```
$ BATS_RUN_SKIPPED=true bats -T palindrome_products.bats
palindrome_products.bats
 ✓ finds the smallest palindrome from single digit factors [31]
 ✓ finds the largest palindrome from single digit factors [46]
 ✓ find the smallest palindrome from double digit factors [33]
 ✓ find the largest palindrome from double digit factors [68]
 ✓ find smallest palindrome from triple digit factors [48]
 ✓ find the largest palindrome from triple digit factors [4082]
 ✓ find smallest palindrome from four digit factors [141]
 ✓ find the largest palindrome from four digit factors [41995]
 ✓ empty result for smallest if no palindrome in the range [129]
 ✓ empty result for largest if no palindrome in the range [23]
 ✓ error result for smallest if min is more than max [24]
 ✓ error result for largest if min is more than max [23]
 ✓ error result for first param [23]
 ✓ smallest product does not use the smallest factor [26940]

14 tests, 0 failures in 75 seconds
```
