use context starter2024

provide: rebase end

invalid-digit-error = "all digits must satisfy 0 <= d < input base"
invalid-input-base-error = "input base must be >= 2"
invalid-output-base-error = "output base must be >= 2"

fun to-decimal(
    ds :: List<NumInteger>, 
    sum :: NumInteger, 
    base :: NumInteger
) -> NumInteger:
  doc: "convert a list of digits and a base to a decimal number"
  cases(List) ds:
    | empty => sum
    | link(d, rest) =>
      ask:
        | (d < 0) or (d >= base) then: raise(invalid-digit-error) 
        | otherwise: to-decimal(rest, (sum * base) + d, base)
      end
  end
where:
  to-decimal([list: ], 0, 2) is 0
  to-decimal([list: 1, 0, 1], 0, 2) is 5
  to-decimal([list: -1], 0, 2) raises invalid-digit-error
end

fun to-output-digits(
    dec :: NumInteger, 
    ds :: List<NumInteger>, 
    base :: NumInteger
) -> List<NumInteger>:
  doc: "convert a decimal number and a base to a list of digits"
  ask:
    | (dec == 0) and is-empty(ds) then: [list: 0]
    | dec == 0 then: ds
    | otherwise:
      rem = num-modulo(dec, base)
      to-output-digits((dec - rem) / base, [list: rem] + ds, base)
  end
where:
  to-output-digits(0, [list: ], 2) is [list: 0]
  to-output-digits(1, [list: ], 2) is [list: 1]
  to-output-digits(6, [list: ], 2) is [list: 1, 1, 0]
end

# ----------------------------------------------------
fun rebase(
    input-base :: NumInteger,
    digits :: List<NumInteger>,
    output-base :: NumInteger
) -> List<NumInteger>:
  ask:
    | input-base < 2 then: raise(invalid-input-base-error)
    | output-base < 2 then: raise(invalid-output-base-error)
    | otherwise:
      decimal = to-decimal(digits, 0, input-base)
      to-output-digits(decimal, [list: ], output-base)
  end
end
