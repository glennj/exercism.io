include std/unittest.e 

include robot-simulator.ex 

set_test_verbosity(TEST_SHOW_ALL)

test_equal("Rotating clockwise changes north to east",
            {0, 0, "east"},
            move({0, 0, "north"}, "R"))
test_equal("Rotating clockwise changes east to south",
            {0, 0, "south"},
            move({0, 0, "east"}, "R"))
test_equal("Rotating clockwise changes south to west",
            {0, 0, "west"},
            move({0, 0, "south"}, "R"))
test_equal("Rotating clockwise changes west to north",
            {0, 0, "north"},
            move({0, 0, "west"}, "R"))
test_equal("Rotating counter-clockwise changes north to west",
            {0, 0, "west"},
            move({0, 0, "north"}, "L"))
test_equal("Rotating counter-clockwise changes west to south",
            {0, 0, "south"},
            move({0, 0, "west"}, "L"))
test_equal("Rotating counter-clockwise changes south to east",
            {0, 0, "east"},
            move({0, 0, "south"}, "L"))
test_equal("Rotating counter-clockwise changes east to north",
            {0, 0, "north"},
            move({0, 0, "east"}, "L"))
test_equal("Moving forward one facing north increments Y",
            {0, 1, "north"},
            move({0, 0, "north"}, "A"))
test_equal("Moving forward one facing south decrements Y",
            {0, -1, "south"},
            move({0, 0, "south"}, "A"))
test_equal("Moving forward one facing east increments X",
            {1, 0, "east"},
            move({0, 0, "east"}, "A"))
test_equal("Moving forward one facing west decrements X",
            {-1, 0, "west"},
            move({0, 0, "west"}, "A"))
test_equal("Follow series of instructions moving east and north from README",
            {9, 4, "west"},
            move({7, 3, "north"}, "RAALAL"))
test_equal("Follow series of instructions moving west and north",
            {-4, 1, "west"},
            move({0, 0, "north"}, "LAAARALA"))
test_equal("Follow series of instructions moving west and south",
            {-3, -8, "south"},
            move({2, -7, "east"}, "RRAAAAALA"))
test_equal("Follow series of instructions moving east and north",
            {11, 5, "north"},
            move({8, 4, "south"}, "LAAARRRALLLL"))

test_report()