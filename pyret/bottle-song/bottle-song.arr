use context starter2024

provide: recite end

nums = [list: "No", "One", "Two", "Three", "Four", "Five", "Six", "Seven", "Eight", "Nine", "Ten"]

fun bottles(n):
  nums.get(n) + " green bottle" + (if n == 1: "" else: "s" end)
end

fun verse(n):
  line1 = bottles(n) + " hanging on the wall,"
  [list: line1, line1, 
    "And if one green bottle should accidentally fall,",
    "There'll be " + string-to-lower(bottles(n - 1)) + " hanging on the wall."
  ].join-str("\n")
end

fun build-song(n, remaining, vs):
  ask:
    | remaining == 0 then: vs.join-str("\n\n")
    | otherwise: build-song(n - 1, remaining - 1, vs + [list: verse(n)])
  end
end

# -------------------------------------------
fun recite(start-bottles, take-down):
  build-song(start-bottles, take-down, [list: ])
end
