use context essentials2020 # Don't delete this line when using Pyret on Exercism 

provide: convert end

fun convert(n):
  drops = if num-modulo(n, 3) == 0: "Pling" else: "" end
        + if num-modulo(n, 5) == 0: "Plang" else: "" end
        + if num-modulo(n, 7) == 0: "Plong" else: "" end
  ask:
    | string-length(drops) > 0 then: drops
    | otherwise: num-to-string(n)
  end
end
