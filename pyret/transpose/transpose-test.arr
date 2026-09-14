use context starter2024

include file("transpose.arr")

check "empty string":
  transpose([list: ]) is [list: ]
end

check "two characters in a row":
  lines = [list: "A1"]
  expected = [list:
    "A",
    "1"]

  transpose(lines) is expected
end

check "two characters in a column":
  lines = [list:
    "A",
    "1"]
  expected = [list: "A1"]

  transpose(lines) is expected
end

check "simple":
  lines = [list:
    "ABC",
    "123"]
  expected = [list:
    "A1",
    "B2",
    "C3"]

  transpose(lines) is expected
end

check "single line":
  lines = [list: "Single line."]
  expected = [list:
    "S",
    "i",
    "n",
    "g",
    "l",
    "e",
    " ",
    "l",
    "i",
    "n",
    "e",
    "."]

  transpose(lines) is expected
end

check "first line longer than second line":
  lines = [list:
    "The fourth line.",
    "The fifth line."]
  expected = [list:
    "TT",
    "hh",
    "ee",
    "  ",
    "ff",
    "oi",
    "uf",
    "rt",
    "th",
    "h ",
    " l",
    "li",
    "in",
    "ne",
    "e.",
    "."]

  transpose(lines) is expected
end

check "second line longer than first line":
  lines = [list:
    "The first line.",
    "The second line."]
  expected = [list:
    "TT",
    "hh",
    "ee",
    "  ",
    "fs",
    "ie",
    "rc",
    "so",
    "tn",
    " d",
    "l ",
    "il",
    "ni",
    "en",
    ".e",
    " ."]

  transpose(lines) is expected
end

check "mixed line length":
  lines = [list:
    "The longest line.",
    "A long line.",
    "A longer line.",
    "A line."]
  expected = [list:
    "TAAA",
    "h   ",
    "elll",
    " ooi",
    "lnnn",
    "ogge",
    "n e.",
    "glr",
    "ei ",
    "snl",
    "tei",
    " .n",
    "l e",
    "i .",
    "n",
    "e",
    "."]

  transpose(lines) is expected
end

check "square":
  lines = [list:
    "HEART",
    "EMBER",
    "ABUSE",
    "RESIN",
    "TREND"]
  expected = [list:
    "HEART",
    "EMBER",
    "ABUSE",
    "RESIN",
    "TREND"]

  transpose(lines) is expected
end

check "rectangle":
  lines = [list:
    "FRACTURE",
    "OUTLINED",
    "BLOOMING",
    "SEPTETTE"]
  expected = [list:
    "FOBS",
    "RULE",
    "ATOP",
    "CLOT",
    "TIME",
    "UNIT",
    "RENT",
    "EDGE"]

  transpose(lines) is expected
end

check "triangle":
  lines = [list:
    "T",
    "EE",
    "AAA",
    "SSSS",
    "EEEEE",
    "RRRRRR"]
  expected = [list:
    "TEASER",
    " EASER",
    "  ASER",
    "   SER",
    "    ER",
    "     R"]

  transpose(lines) is expected
end

check "jagged triangle":
  lines = [list:
    "11",
    "2",
    "3333",
    "444",
    "555555",
    "66666"]
  expected = [list:
    "123456",
    "1 3456",
    "  3456",
    "  3 56",
    "    56",
    "    5"]

  transpose(lines) is expected
end
