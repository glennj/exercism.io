use context starter2024

provide: my-rotate end

include file("list-helpers.arr")  # for `index-of` function

uppers = string-explode("ABCDEFGHIJKLMNOPQRSTUVWXYZ")
lowers = uppers.map(string-to-lower)
alphabet = uppers + lowers

fun my-rotate(phrase, shift-key):
  rotation = uppers.drop(shift-key)
    + uppers.take(shift-key) 
    + lowers.drop(shift-key)
    + lowers.take(shift-key)

  translate-char = lam(c):
    idx = index-of(alphabet, c)
    ask:
      | idx == -1 then: c
      | otherwise: rotation.get(idx)
    end
  end

  string-explode(phrase).map(translate-char).join-str("")
end
