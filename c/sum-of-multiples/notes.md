# Progression through the sum-of-multiples exercise

## Take 1

My first take was a simple one:

* foreach number `i` between 1 and `limit`
	* if there is _some_ factor that divides `i`
		* add `i` to the total

This is a straightforward implementation, and does not require any dynamic allocations.
However, it is very inefficient: it runs in `O(limit * number_of_factors)`

## Take 2

The second take was to allocate an array of `uint8_t` of length `limit`:
store `1` for each `i` that is a multiple

* foreach factor `f` in `factors`
	* if `f` is not zero
		* foreach multiple of `f` smaller than `limit`
			* set `multiples[mult]` to `1`

```none
$ make clean; make test >/dev/null; valgrind ./tests.out
rm -rf *.o *.out *.out.dSYM
==72595== Memcheck, a memory error detector
==72595== Copyright (C) 2002-2022, and GNU GPL'd, by Julian Seward et al.
==72595== Using Valgrind-3.19.0 and LibVEX; rerun with -h for copyright info
==72595== Command: ./tests.out
==72595==
test_sum_of_multiples.c:134:test_no_multiples_within_limit:PASS
test_sum_of_multiples.c:135:test_one_factor_has_multiples_within_limit:PASS
test_sum_of_multiples.c:136:test_more_than_one_multiple_withiin_limit:PASS
test_sum_of_multiples.c:137:test_more_than_one_factor_with_multiples_within_limit:PASS
test_sum_of_multiples.c:138:test_each_multiple_is_only_counted_once:PASS
test_sum_of_multiples.c:139:test_a_much_larger_limit:PASS
test_sum_of_multiples.c:140:test_three_factors:PASS
test_sum_of_multiples.c:141:test_factors_not_relatively_prime:PASS
test_sum_of_multiples.c:142:test_some_pairs_of_factors_relatively_prime_and_some_not:PASS
test_sum_of_multiples.c:143:test_one_factor_is_a_multiple_of_another:PASS
test_sum_of_multiples.c:144:test_much_larger_factors:PASS
test_sum_of_multiples.c:145:test_all_numbers_are_multiples_of_1:PASS
test_sum_of_multiples.c:146:test_no_factors_means_an_empty_sum:PASS
test_sum_of_multiples.c:147:test_the_only_multiple_of_zero_is_zero:PASS
test_sum_of_multiples.c:148:test_the_factor_0_does_not_affect_the_sum_of_multiples_of_other_factors:PASS
test_sum_of_multiples.c:150:test_solutions_using_include_exclude_must_extend_to_cardinality_greater_than_3:PASS

-----------------------
16 Tests 0 Failures 0 Ignored
OK
==72595==
==72595== HEAP SUMMARY:
==72595==     in use at exit: 0 bytes in 0 blocks
==72595==   total heap usage: 17 allocs, 17 frees, 23,487 bytes allocated
==72595==
==72595== All heap blocks were freed -- no leaks are possible
==72595==
==72595== For lists of detected and suppressed errors, rerun with: -s
==72595== ERROR SUMMARY: 0 errors from 0 contexts (suppressed: 0 from 0)
```

## Take 3

But then I thought "8 bits to store a 0/1 value is pretty wasteful."
Searching for "bit vectors in c" revealed [https://web.cs.dal.ca/~jamie/UWO/BitVectors/README.html](https://web.cs.dal.ca/~jamie/UWO/BitVectors/README.html)

I brought that library in, and added a variable to control it's use during compilation.
Addition to `makefile`:
```makefile
USE_BIT_ARRAY = 0
CFLAGS += -DUSE_BIT_ARRAY=$(USE_BIT_ARRAY)
```

Then

```none
$ make clean; make USE_BIT_ARRAY=1 test >/dev/null; valgrind ./tests.out
rm -rf *.o *.out *.out.dSYM
==72677== Memcheck, a memory error detector
==72677== Copyright (C) 2002-2022, and GNU GPL'd, by Julian Seward et al.
==72677== Using Valgrind-3.19.0 and LibVEX; rerun with -h for copyright info
==72677== Command: ./tests.out
==72677==
test_sum_of_multiples.c:134:test_no_multiples_within_limit:PASS
test_sum_of_multiples.c:135:test_one_factor_has_multiples_within_limit:PASS
test_sum_of_multiples.c:136:test_more_than_one_multiple_withiin_limit:PASS
test_sum_of_multiples.c:137:test_more_than_one_factor_with_multiples_within_limit:PASS
test_sum_of_multiples.c:138:test_each_multiple_is_only_counted_once:PASS
test_sum_of_multiples.c:139:test_a_much_larger_limit:PASS
test_sum_of_multiples.c:140:test_three_factors:PASS
test_sum_of_multiples.c:141:test_factors_not_relatively_prime:PASS
test_sum_of_multiples.c:142:test_some_pairs_of_factors_relatively_prime_and_some_not:PASS
test_sum_of_multiples.c:143:test_one_factor_is_a_multiple_of_another:PASS
test_sum_of_multiples.c:144:test_much_larger_factors:PASS
test_sum_of_multiples.c:145:test_all_numbers_are_multiples_of_1:PASS
test_sum_of_multiples.c:146:test_no_factors_means_an_empty_sum:PASS
test_sum_of_multiples.c:147:test_the_only_multiple_of_zero_is_zero:PASS
test_sum_of_multiples.c:148:test_the_factor_0_does_not_affect_the_sum_of_multiples_of_other_factors:PASS
test_sum_of_multiples.c:150:test_solutions_using_include_exclude_must_extend_to_cardinality_greater_than_3:PASS

-----------------------
16 Tests 0 Failures 0 Ignored
OK
==72677==
==72677== HEAP SUMMARY:
==72677==     in use at exit: 0 bytes in 0 blocks
==72677==   total heap usage: 17 allocs, 17 frees, 3,838 bytes allocated
==72677==
==72677== All heap blocks were freed -- no leaks are possible
==72677==
==72677== For lists of detected and suppressed errors, rerun with: -s
==72677== ERROR SUMMARY: 0 errors from 0 contexts (suppressed: 0 from 0)
```

Note the total heap usage, before using a bit array and after:
```none
==72595==   total heap usage: 17 allocs, 17 frees, 23,487 bytes allocated
==72677==   total heap usage: 17 allocs, 17 frees, 3,838 bytes allocated
```
