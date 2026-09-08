use context starter2024

provide: is-valid end

fun isbn10-sum(cs :: List<String>, idx :: Number, sum :: Number) -> Option<Number>:
  cases(List) cs:
    | empty => if idx == 10: some(sum) else: none end
    | link(c, rest) =>
      if (c == "X") and (idx == 9):
        isbn10-sum(rest, idx + 1, sum + 10)
      else:
        cases(Option) string-to-number(c):
          | none => none
          | some(n) => isbn10-sum(rest, idx + 1, sum + (n * (10 - idx)))
        end
      end
  end
end

# ----------------------------------------
fun is-valid(isbn :: String) -> Boolean:
  cleaned = string-explode(string-replace(isbn, "-", ""))
  cases(Option) isbn10-sum(cleaned, 0, 0):
    | none => false
    | some(sum) => num-modulo(sum, 11) == 0
  end
end
