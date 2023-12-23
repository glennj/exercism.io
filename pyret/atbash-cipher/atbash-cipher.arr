use context essentials2020 # Don't delete this line when using Pyret on Exercism 

provide: encode, decode end

fun decode(phrase):
  is-lower = {(cp): (cp >= 97) and (cp <= 122)}
  is-alnum = {(cp): is-lower(cp) or ((cp >= 48) and (cp <= 57))}

  encipher = {(cp): if is-lower(cp): 122 - (cp - 97) else: cp end}

  phrase
  ^ string-to-lower(_)
  ^ string-to-code-points(_)
  ^ filter(is-alnum, _)
  ^ map(encipher, _)
  ^ string-from-code-points(_)
end


fun encode(phrase):
  decode(phrase) ^ grouped(_, 5)
end

fun grouped(str, size):
  rec helper = lam(s, result, sep):
    ask:
      | string-length(s) <= size then: result + sep + s
      | otherwise:
          helper(string-substring(s, size, string-length(s)),
                 result + sep + string-substring(s, 0, size),
                 " ")
    end
  end
  helper(str, "", "")
end
