provide:
  num-shr,
  num-and1
end

fun num-shr(n :: NumNonNegative, bits :: NumNonNegative):
  doc: ```
    Shift number `n` right by `bits` places.
  ```
  num-floor(n / (num-expt(2, bits)))
where:
  num-shr(4, 1) is 2
  num-shr(15, 2) is 3
end

fun num-and1(n :: NumNonNegative):
  doc: ```
    Result of ANDing number `n` and `1`.
  ```
  num-modulo(n, 2)
where:
  num-and1(4) is 0
  num-and1(5) is 1
end
