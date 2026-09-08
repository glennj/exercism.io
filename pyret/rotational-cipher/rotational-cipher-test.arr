use context starter2024

include file("rotational-cipher.arr")

check "rotate a by 0, same output as input":
  my-rotate("a", 0) is "a"
end

check "rotate a by 1":
  my-rotate("a", 1) is "b"
end

check "rotate a by 26, same output as input":
  my-rotate("a", 26) is "a"
end

check "rotate m by 13":
  my-rotate("m", 13) is "z"
end

check "rotate n by 13 with wrap around alphabet":
  my-rotate("n", 13) is "a"
end

check "rotate capital letters":
  my-rotate("OMG", 5) is "TRL"
end

check "rotate spaces":
  my-rotate("O M G", 5) is "T R L"
end

check "rotate numbers":
  my-rotate("Testing 1 2 3 testing", 4) is "Xiwxmrk 1 2 3 xiwxmrk"
end

check "rotate punctuation":
  my-rotate("Let's eat, Grandma!", 21) is "Gzo'n zvo, Bmviyhv!"
end

check "rotate all letters":
  my-rotate("The quick brown fox jumps over the lazy dog.", 13) is "Gur dhvpx oebja sbk whzcf bire gur ynml qbt."
end
