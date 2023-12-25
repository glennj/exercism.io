use context essentials2020

include file("roman-numerals.arr")

#|
  When working offline, all tests except the first one are skipped by default.
  Once you get the first test running, unskip the next one until all tests pass locally.
  Check the block comment below for further details.
|#

fun I():
  check "1 is I":
    to-roman(1) is "I"
  end
end

fun II():
  check "2 is II":
    to-roman(2) is "II"
  end
end

fun III():
  check "3 is III":
    to-roman(3) is "III"
  end
end

fun IV():
  check "4 is IV":
    to-roman(4) is "IV"
  end
end

fun V():
  check "5 is V":
    to-roman(5) is "V"
  end
end

fun VI():
  check "6 is VI":
    to-roman(6) is "VI"
  end
end

fun IX():
  check "9 is IX":
    to-roman(9) is "IX"
  end
end

fun XVI():
  check "16 is XVI":
    to-roman(16) is "XVI"
  end
end

fun XXVII():
  check "27 is XXVII":
    to-roman(27) is "XXVII"
  end
end

fun XLVIII():
  check "48 is XLVIII":
    to-roman(48) is "XLVIII"
  end
end

fun XLIX():
  check "49 is XLIX":
    to-roman(49) is "XLIX"
  end
end

fun LIX():
  check "59 is LIX":
    to-roman(59) is "LIX"
  end
end

fun LXVI():
  check "66 is LXVI":
    to-roman(66) is "LXVI"
  end
end

fun XCIII():
  check "93 is XCIII":
    to-roman(93) is "XCIII"
  end
end

fun CXLI():
  check "141 is CXLI":
    to-roman(141) is "CXLI"
  end
end

fun CLXIII():
  check "163 is CLXIII":
    to-roman(163) is "CLXIII"
  end
end

fun CLXVI():
  check "166 is CLXVI":
    to-roman(166) is "CLXVI"
  end
end

fun CDII():
  check "402 is CDII":
    to-roman(402) is "CDII"
  end
end

fun DLXXV():
  check "575 is DLXXV":
    to-roman(575) is "DLXXV"
  end
end

fun DCLXVI():
  check "666 is DCLXVI":
    to-roman(666) is "DCLXVI"
  end
end

fun CMXI():
  check "911 is CMXI":
    to-roman(911) is "CMXI"
  end
end

fun MXXIV():
  check "1024 is MXXIV":
    to-roman(1024) is "MXXIV"
  end
end

fun MDCLXVI():
  check "1666 is MDCLXVI":
    to-roman(1666) is "MDCLXVI"
  end
end

fun MMM():
  check "3000 is MMM":
    to-roman(3000) is "MMM"
  end
end

fun MMMI():
  check "3001 is MMMI":
    to-roman(3001) is "MMMI"
  end
end

fun MMMCMXCIX():
  check "3999 is MMMCMXCIX":
    to-roman(3999) is "MMMCMXCIX"
  end
end


#|
  Code to run each test. Each line corresponds to a test above and whether it should be run.
  To mark a test to be run, replace `false` with `true` on that same line after the comma.
  test(test-a, true) will be run. test(test-a, true) will be skipped.
|#

data TestRun: test(run, active) end

[list: 
  test(I, true),
  test(II, true),
  test(III, true),
  test(IV, true),
  test(V, true),
  test(VI, true),
  test(IX, true),
  test(XVI, true),
  test(XXVII, true),
  test(XLVIII, true),
  test(XLIX, true),
  test(LIX, true),
  test(LXVI, true),
  test(XCIII, true),
  test(CXLI, true),
  test(CLXIII, true),
  test(CLXVI, true),
  test(CDII, true),
  test(DLXXV, true),
  test(DCLXVI, true),
  test(CMXI, true),
  test(MXXIV, true),
  test(MDCLXVI, true),
  test(MMM, true),
  test(MMMI, true),
  test(MMMCMXCIX, true)
].each(lam(t): when t.active: t.run() end end)
