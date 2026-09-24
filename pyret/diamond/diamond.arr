use context starter2024

import lists as L

provide: rows end

alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
mirror = lam(lst): lst + lst.reverse().drop(1) end

fun rows(letter):
  n = string-find(alphabet, letter) + 1
  top-half = range(0, n).map(lam(i):
    c = string-char-at(alphabet, i)
    spaces = L.build-list(lam(_): " " end, n)
    mirror(spaces.set(n - (i + 1), c)).join-str("")
  end)
  mirror(top-half)
end
