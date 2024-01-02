use context essentials2020

include file("dnd-character.arr")

#|
  When working offline, all tests except the first one are skipped by default.
  Once you get the first test running, unskip the next one until all tests pass locally.
  Check the block comment below for further details.
|#


# declaring some variables to be more concise inside the tests.
modifier = blank-character().modifier 
ability = blank-character().ability
randomize-stats = blank-character().randomize-stats

fun modifier-score-3():
  check "ability modifier -> ability modifier for score 3 is -4":
    modifier(3) is -4
  end
end

fun modifier-score-4():
  check "ability modifier -> ability modifier for score 4 is -3":
    modifier(4) is -3
  end
end

fun modifier-score-5():
  check "ability modifier -> ability modifier for score 5 is -3":
    modifier(5) is -3
  end
end

fun modifier-score-6():
  check "ability modifier -> ability modifier for score 6 is -2":
    modifier(6) is -2
  end
end

fun modifier-score-7():
  check "ability modifier -> ability modifier for score 7 is -2":
    modifier(7) is -2
  end
end

fun modifier-score-8():
  check "ability modifier -> ability modifier for score 8 is -1":
    modifier(8) is -1
  end
end

fun modifier-score-9():
  check "ability modifier -> ability modifier for score 9 is -1":
    modifier(9) is -1
  end
end

fun modifier-score-10():
  check "ability modifier -> ability modifier for score 10 is 0":
    modifier(10) is 0
  end
end

fun modifier-score-11():
  check "ability modifier -> ability modifier for score 11 is 0":
    modifier(11) is 0
  end
end

fun modifier-score-12():
  check "ability modifier -> ability modifier for score 12 is +1":
    modifier(12) is 1
  end
end

fun modifier-score-13():
  check "ability modifier -> ability modifier for score 13 is +1":
    modifier(13) is 1
  end
end

fun modifier-score-14():
  check "ability modifier -> ability modifier for score 14 is +2":
    modifier(14) is 2
  end
end

fun modifier-score-15():
  check "ability modifier -> ability modifier for score 15 is +2":
    modifier(15) is 2
  end
end

fun modifier-score-16():
  check "ability modifier -> ability modifier for score 16 is +3":
    modifier(16) is 3
  end
end

fun modifier-score-17():
  check "ability modifier -> ability modifier for score 17 is +3":
    modifier(17) is 3
  end
end

fun modifier-score-18():
  check "ability modifier -> ability modifier for score 18 is +4":
    modifier(18) is 4
  end
end

fun ability-within-range():
  check "random ability is within range":
      stat = ability()
  
      is-valid = lam(n): (n >= 3) and (n <= 18) end

      is-valid(stat) is true
  end
end

fun random-character-is-valid():
  check "random character is valid":
    new-character = randomize-stats()

    is-valid = lam(n): (n >= 3) and (n <= 18) end

    is-valid(new-character.strength) is true
    is-valid(new-character.dexterity) is true
    is-valid(new-character.constitution) is true
    is-valid(new-character.intelligence) is true
    is-valid(new-character.wisdom) is true
    is-valid(new-character.charisma) is true

    expected = 10 + new-character.modifier(new-character.constitution)
    new-character.get-hitpoints() is expected
  end
end

#|
  Code to run each test. Each line corresponds to a test above and whether it should be run.
  To mark a test to be run, replace `false` with `true` on that same line after the comma.
  test(test-a, true) will be run. test(test-a, true) will be skipped.
|#

data TestRun: test(run, active) end

[list: 
  test(modifier-score-3, true),
  test(modifier-score-4, true),
  test(modifier-score-5, true),
  test(modifier-score-6, true),
  test(modifier-score-7, true),
  test(modifier-score-8, true),
  test(modifier-score-9, true),
  test(modifier-score-10, true),
  test(modifier-score-11, true),
  test(modifier-score-12, true),
  test(modifier-score-13, true),
  test(modifier-score-14, true),
  test(modifier-score-15, true),
  test(modifier-score-16, true),
  test(modifier-score-17, true),
  test(modifier-score-18, true),
  test(ability-within-range, true),
  test(random-character-is-valid, true)
].each(lam(t): when t.active: t.run() end end)
