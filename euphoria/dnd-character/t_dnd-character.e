include std/unittest.e

include dnd-character.ex

set_test_verbosity(TEST_SHOW_ALL)

test_equal("ability modifier for score 3 is -4",
           -4,
           modifier(3))
test_equal("ability modifier for score 4 is -3",
           -3,
           modifier(4))
test_equal("ability modifier for score 5 is -3",
           -3,
           modifier(5))
test_equal("ability modifier for score 6 is -2",
           -2,
           modifier(6))
test_equal("ability modifier for score 7 is -2",
           -2,
           modifier(7))
test_equal("ability modifier for score 8 is -1",
           -1,
           modifier(8))
test_equal("ability modifier for score 9 is -1",
           -1,
           modifier(9))
test_equal("ability modifier for score 10 is 0",
           0,
           modifier(10))
test_equal("ability modifier for score 11 is 0",
           0,
           modifier(11))
test_equal("ability modifier for score 12 is +1",
           1,
           modifier(12))
test_equal("ability modifier for score 13 is +1",
           1,
           modifier(13))
test_equal("ability modifier for score 14 is +2",
           2,
           modifier(14))
test_equal("ability modifier for score 15 is +2",
           2,
           modifier(15))
test_equal("ability modifier for score 16 is +3",
           3,
           modifier(16))
test_equal("ability modifier for score 17 is +3",
           3,
           modifier(17))
test_equal("ability modifier for score 18 is +4",
           4,
           modifier(18))
test_in_range("random ability is within range",
              ability())
test_valid_character()
test_ability_calculated_once()

test_report()

procedure test_valid_character()
  sequence char = new_character()
  test_in_range("random character is valid - strength",
                get_strength(char))
  test_in_range("random character is valid - dexterity",
                get_dexterity(char))
  test_in_range("random character is valid - constitution",
                get_constitution(char))
  test_in_range("random character is valid - intelligence",
                get_intelligence(char))
  test_in_range("random character is valid - wisdom",
                get_wisdom(char))
  test_in_range("random character is valid - charisma",
                get_charisma(char))
  test_equal("random character is valid - hitpoints",
             10 + modifier(get_constitution(char)),
             get_hitpoints(char))
end procedure

procedure test_in_range(sequence desc, integer val)
  test_true(desc, val >= 3 and val <= 18)
end procedure

procedure test_ability_calculated_once()
  sequence char = new_character()
  test_equal("each ability is calculated once - strength",
             get_strength(char),
             get_strength(char))
  test_equal("each ability is calculated once - dexterity",
             get_dexterity(char),
             get_dexterity(char))
  test_equal("each ability is calculated once - constitution",
             get_constitution(char),
             get_constitution(char))
  test_equal("each ability is calculated once - intelligence",
             get_intelligence(char),
             get_intelligence(char))
  test_equal("each ability is calculated once - wisdom",
             get_wisdom(char),
             get_wisdom(char))
  test_equal("each ability is calculated once - charisma",
             get_charisma(char),
             get_charisma(char))
end procedure
