use context essentials2020 # Don't delete this line when using Pyret on Exercism 

provide: slices end

fun slices(series, len) block:
  slen = string-length(series)
  ask:
    | slen == 0  then: raise("series cannot be empty")
    | len > slen then: raise("slice length cannot be greater than series length")
    | len == 0   then: raise("slice length cannot be zero")
    | len < 0    then: raise("slice length cannot be negative")
    | otherwise:
        range(0, (slen - len) + 1)
          .map(lam(i): string-substring(series, i, i + len) end)
  end
end
