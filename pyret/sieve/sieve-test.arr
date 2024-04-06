use context essentials2020

include file("sieve.arr")

#|
  When working offline, all tests except the first one are skipped by default.
  Once you get the first test running, unskip the next one until all tests pass locally.
  Check the block comment below for further details.
|#

fun no-primes-under-two():
  check "no primes under two":
    primes(1) is [list:]
  end
end
  
 fun find-first-prime():
  check "find first prime":
    primes(2) is [list:2]
  end
end

fun find-primes-up-to-10():
  check "find primes up to 10":
    primes(10) is [list:2, 3, 5, 7]
  end
end

fun limit-is-prime():
  check "limit is prime":
    primes(13) is [list:2, 3, 5, 7, 11, 13]
  end
end

fun find-primes-up-to-1000():
  check "find primes up to 1000":
    primes(1000) is [list:
          2,   3,   5,   7,  11,  13,  17,  19,  23,  29,  31,  37,  41,  43,
         47,  53,  59,  61,  67,  71,  73,  79,  83,  89,  97, 101, 103, 107,
        109, 113, 127, 131, 137, 139, 149, 151, 157, 163, 167, 173, 179, 181,
        191, 193, 197, 199, 211, 223, 227, 229, 233, 239, 241, 251, 257, 263,
        269, 271, 277, 281, 283, 293, 307, 311, 313, 317, 331, 337, 347, 349,
        353, 359, 367, 373, 379, 383, 389, 397, 401, 409, 419, 421, 431, 433,
        439, 443, 449, 457, 461, 463, 467, 479, 487, 491, 499, 503, 509, 521,
        523, 541, 547, 557, 563, 569, 571, 577, 587, 593, 599, 601, 607, 613,
        617, 619, 631, 641, 643, 647, 653, 659, 661, 673, 677, 683, 691, 701,
        709, 719, 727, 733, 739, 743, 751, 757, 761, 769, 773, 787, 797, 809,
        811, 821, 823, 827, 829, 839, 853, 857, 859, 863, 877, 881, 883, 887,
        907, 911, 919, 929, 937, 941, 947, 953, 967, 971, 977, 983, 991, 997
    ]
  end
end

#|
  Code to run each test. Each line corresponds to a test above and whether it should be run.
  To mark a test to be run, replace `false` with `true` on that same line after the comma.
  test(test-a, true) will be run. test(test-a, true) will be skipped.
|#

data TestRun: test(run, active) end

[list: 
  test(no-primes-under-two, true),
  test(find-first-prime, true),
  test(find-primes-up-to-10, true),
  test(limit-is-prime, true),
  test(find-primes-up-to-1000, false),
].each(lam(t): when t.active: t.run() end end)
