use context starter2024

include file("flower-field.arr")

check "no rows":
  garden = [list: ]
  expected = [list: ]

  annotate(garden) is expected
end

check "no columns":
  garden = [list: ""]
  expected = [list: ""]

  annotate(garden) is expected
end

check "no flowers":
  garden = [list:
    "   ",
    "   ",
    "   "]
  expected = [list:
    "   ",
    "   ",
    "   "]

  annotate(garden) is expected
end

check "garden full of flowers":
  garden = [list:
    "***",
    "***",
    "***"]
  expected = [list:
    "***",
    "***",
    "***"]

  annotate(garden) is expected
end

check "flower surrounded by spaces":
  garden = [list:
    "   ",
    " * ",
    "   "]
  expected = [list:
    "111",
    "1*1",
    "111"]

  annotate(garden) is expected
end

check "space surrounded by flowers":
  garden = [list:
    "***",
    "* *",
    "***"]
  expected = [list:
    "***",
    "*8*",
    "***"]

  annotate(garden) is expected
end

check "horizontal line":
  garden = [list: " * * "]
  expected = [list: "1*2*1"]

  annotate(garden) is expected
end

check "horizontal line, flowers at edges":
  garden = [list: "*   *"]
  expected = [list: "*1 1*"]

  annotate(garden) is expected
end

check "vertical line":
  garden = [list:
    " ",
    "*",
    " ",
    "*",
    " "]
  expected = [list:
    "1",
    "*",
    "2",
    "*",
    "1"]

  annotate(garden) is expected
end

check "vertical line, flowers at edges":
  garden = [list:
    "*",
    " ",
    " ",
    " ",
    "*"]
  expected = [list:
    "*",
    "1",
    " ",
    "1",
    "*"]

  annotate(garden) is expected
end

check "cross":
  garden = [list:
    "  *  ",
    "  *  ",
    "*****",
    "  *  ",
    "  *  "]
  expected = [list:
    " 2*2 ",
    "25*52",
    "*****",
    "25*52",
    " 2*2 "]

  annotate(garden) is expected
end

check "large garden":
  garden = [list:
    " *  * ",
    "  *   ",
    "    * ",
    "   * *",
    " *  * ",
    "      "]
  expected = [list:
    "1*22*1",
    "12*322",
    " 123*2",
    "112*4*",
    "1*22*2",
    "111111"]

  annotate(garden) is expected
end

check "multiple adjacent flowers":
  garden = [list: " ** "]
  expected = [list: "1**1"]

  annotate(garden) is expected
end
