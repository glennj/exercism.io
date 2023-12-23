use context essentials2020

include file("resistor-color-trio.arr")

#|
  When working offline, all tests except the first one are skipped by default.
  Once you get the first test running, unskip the next one until all tests pass locally.
  Check the block comment below for further details.
|#

fun orange-orange-black():
  check "Orange and orange and black":
    colors = [list: "orange", "orange", "black"]
    expected = "33 ohms"

    label(colors) is expected
  end
end

fun blue-grey-brown():
  check "Blue and grey and brown":
    colors = [list: "blue", "grey", "brown"]
    expected = "680 ohms"

    label(colors) is expected
  end
end

fun red-black-red():
  check "Red and black and red":
    colors = [list: "red", "black", "red"]
    expected = "2 kiloohms"

    label(colors) is expected
  end
end

fun green-brown-orange():
  check "Green and brown and orange":
    colors = [list: "green", "brown", "orange"]
    expected = "51 kiloohms"

    label(colors) is expected
  end
end

fun yellow-violet-yellow():
  check "Yellow and violet and yellow":
    colors = [list: "yellow", "violet", "yellow"]
    expected = "470 kiloohms"

    label(colors) is expected
  end
end

fun blue-violet-blue():
  check "Blue and violet and blue":
    colors = [list: "blue", "violet", "blue"]
    expected = "67 megaohms"

    label(colors) is expected
  end
end

fun minimum-value():
  check "Minimum possible value":
    colors = [list: "black", "black", "black"]
    expected = "0 ohms"

    label(colors) is expected
  end
end

fun maximum-value():
  check "Maximum possible value":
    colors = [list: "white", "white", "white"]
    expected = "99 gigaohms"

    label(colors) is expected
  end
end

fun invalid-octal-number():
  check "First two colors make an invalid octal number":
    colors = [list: "black", "grey", "black"]
    expected = "8 ohms"

    label(colors) is expected
  end
end

fun ignore-extra-colors():
  check "Ignore extra colors":
    colors = [list: "blue", "green", "yellow", "orange"]
    expected = "650 kiloohms"

    label(colors) is expected
  end
end

#|
  Code to run each test. Each line corresponds to a test above and whether it should be run.
  To mark a test to be run, replace `false` with `true` on that same line after the comma.
  test(test-a, true) will be run. test(test-a, false) will be skipped.
|#

data TestRun: test(run, active) end

[list: 
  test(orange-orange-black, true),
  test(blue-grey-brown, true),
  test(red-black-red, true),
  test(green-brown-orange, true),
  test(yellow-violet-yellow, true),
  test(blue-violet-blue, true),
  test(minimum-value, true),
  test(maximum-value, true),
  test(invalid-octal-number, true),
  test(ignore-extra-colors, true)
].each(lam(t): when t.active: t.run() end end)
