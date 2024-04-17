include std/math.e
include std/unittest.e 
include std/types.e

include strain.ex 

set_test_verbosity(TEST_SHOW_ALL)

test_equal("keep on empty list returns empty list",
            {},
            keep({},
                 routine_id("always_true")))
test_equal("keeps everything",
            {1, 3, 5},
            keep({1, 3, 5},
                 routine_id("always_true")))
test_equal("keeps nothing",
            {},
            keep({1, 3, 5},
                 routine_id("always_false")))
test_equal("keeps first and last",
            {1, 3},
            keep({1, 2, 3},
                 routine_id("is_odd")))
test_equal("keeps neither first nor last",
            {2},
            keep({1, 2, 3},
                 routine_id("is_even")))
test_equal("keeps strings",
            {"zebra", "zombies", "zealot"},
            keep({"apple", "zebra", "banana", "zombies", "cherimoya", "zealot"},
                 routine_id("starts_with_z")))
test_equal("keeps lists",
            {{5, 5, 5},
             {5, 1, 2},
             {1, 5, 2},
             {1, 2, 5}},
            keep({{1, 2, 3},
                  {5, 5, 5},
                  {5, 1, 2},
                  {2, 1, 2},
                  {1, 5, 2},
                  {2, 2, 1},
                  {1, 2, 5}},
                routine_id("contains_five")))

test_equal("discard on empty list returns empty list",
            {},
            discard({},
                    routine_id("always_true")))
test_equal("discards everything",
            {},
            discard({1, 3, 5},
                    routine_id("always_true")))
test_equal("discards nothing",
            {1, 3, 5},
            discard({1, 3, 5},
                    routine_id("always_false")))
test_equal("discards first and last",
            {2},
            discard({1, 2, 3},
                    routine_id("is_odd")))
test_equal("discards neither first nor last",
            {1, 3},
            discard({1, 2, 3},
                    routine_id("is_even")))
test_equal("discards strings",
            {"apple", "banana", "cherimoya"},
            discard({"apple", "zebra", "banana", "zombies", "cherimoya", "zealot"},
                    routine_id("starts_with_z")))
test_equal("discards lists",
            {{1, 2, 3},
             {2, 1, 2},
             {2, 2, 1}},
            discard({{1, 2, 3},
                     {5, 5, 5},
                     {5, 1, 2},
                     {2, 1, 2},
                     {1, 5, 2},
                     {2, 2, 1},
                     {1, 2, 5}},
                    routine_id("contains_five")))

test_report()

function always_true(integer x)
    return TRUE
end function

function always_false(integer x)
    return FALSE
end function

function is_odd(integer x)
    return mod(x, 2) = 1
end function

function is_even(integer x)
    return mod(x, 2) = 0
end function

function starts_with_z(sequence x)
    return find('z', x) = 1
end function

function contains_five(sequence x)
    return find(5, x) != 0
end function
