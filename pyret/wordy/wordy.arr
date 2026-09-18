use context starter2024

include string-dict

provide: answer end

operations = [string-dict:
  "plus",          lam(a, b): a + b end,
  "minus",         lam(a, b): a - b end,
  "multiplied-by", lam(a, b): a * b end,
  "divided-by",    lam(a, b): num-floor(a / b) end
]

fun answer(question):
  tokens = question
    ^ string-to-lower(_)
    ^ string-replace(_, "?", "")
    ^ string-replace(_, " by", "-by")
    ^ string-split-all(_, " ")

  ask:
    | (tokens.take(2) <> [list: "what","is"]) then: raise("unknown operation")
    | otherwise: parse-number(tokens.drop(2), 0, "plus")
  end
end

# ---------------------------------------------------
fun parse-number(tokens, current-result, op):
  cases(List) tokens:
    | empty => raise("syntax error")
    | link(token, rest) => 
        cases(Option) string-to-number(token):
          | none => raise("syntax error")
          | some(n) =>
              f = operations.get-value(op)
              parse-operator(rest, f(current-result, n))
        end
  end
end

fun parse-operator(tokens, current-result):
  cases(List) tokens:
    | empty => current-result
    | link(token, rest) => 
        if operations.has-key(token):
          parse-number(rest, current-result, token)
        else:
          cases(Option) string-to-number(token):
            | some(_) => raise("syntax error")
            | none => raise("unknown operation")
          end
        end
  end
end
