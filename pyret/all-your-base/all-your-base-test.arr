use context starter2024

include file("all-your-base.arr")

check "single bit one to decimal":
  rebase(2, [list: 1], 10) is [list: 1]
end

check "binary to single decimal":
  rebase(2, [list: 1, 0, 1], 10) is [list: 5]
end

check "single decimal to binary":
  rebase(10, [list: 5], 2) is [list: 1, 0, 1]
end

check "binary to multiple decimal":
  rebase(2, [list: 1, 0, 1, 0, 1, 0], 10) is [list: 4, 2]
end

check "decimal to binary":
  rebase(10, [list: 4, 2], 2) is [list: 1, 0, 1, 0, 1, 0]
end

check "trinary to hexadecimal":
  rebase(3, [list: 1, 1, 2, 0], 16) is [list: 2, 10]
end

check "hexadecimal to trinary":
  rebase(16, [list: 2, 10], 3) is [list: 1, 1, 2, 0]
end

check "15-bit integer":
  rebase(97, [list: 3, 46, 60], 73) is [list: 6, 10, 45]
end

check "empty list":
  rebase(2, [list: ], 10) is [list: 0]
end

check "single zero":
  rebase(10, [list: 0], 2) is [list: 0]
end

check "multiple zeros":
  rebase(10, [list: 0, 0, 0], 2) is [list: 0]
end

check "leading zeros":
  rebase(7, [list: 0, 6, 0], 10) is [list: 4, 2]
end

check "input base is one":
  rebase(1, [list: 0], 10) raises "input base must be >= 2"
end

check "input base is zero":
  rebase(0, [list: ], 10) raises "input base must be >= 2"
end

check "input base is negative":
  rebase(-2, [list: 1], 10) raises "input base must be >= 2"
end

check "negative digit":
  rebase(2, [list: 1, -1, 1, 0, 1, 0], 10) raises "all digits must satisfy 0 <= d < input base"
end

check "invalid positive digit":
  rebase(2, [list: 1, 2, 1, 0, 1, 0], 10) raises "all digits must satisfy 0 <= d < input base"
end

check "output base is one":
  rebase(2, [list: 1, 0, 1, 0, 1, 0], 1) raises "output base must be >= 2"
end

check "output base is zero":
  rebase(10, [list: 7], 0) raises "output base must be >= 2"
end

check "output base is negative":
  rebase(2, [list: 1], -7) raises "output base must be >= 2"
end

check "both bases are negative":
  rebase(-2, [list: 1], -7) raises "input base must be >= 2"
end
