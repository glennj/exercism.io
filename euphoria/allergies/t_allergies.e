include std/unittest.e

include allergies.ex

set_test_verbosity(TEST_SHOW_ALL)

test_false("not allergic to anything", allergicTo("eggs", 0))
test_true("allergic only to eggs", allergicTo("eggs", 1))
test_true("allergic to eggs and something else", allergicTo("eggs", 3))
test_false("allergic to something, but not eggs", allergicTo("eggs", 2))
test_true("allergic to everything", allergicTo("eggs", 255))

test_false("not allergic to anything", allergicTo("peanuts", 0))
test_true("allergic only to peanuts", allergicTo("peanuts", 2))
test_true("allergic to peanuts and something else", allergicTo("peanuts", 7))
test_false("allergic to something, but not peanuts", allergicTo("peanuts", 5))
test_true("allergic to everything", allergicTo("peanuts", 255))

test_false("not allergic to anything", allergicTo("shellfish", 0))
test_true("allergic only to shellfish", allergicTo("shellfish", 4))
test_true("allergic to shellfish and something else", allergicTo("shellfish", 14))
test_false("allergic to something, but not shellfish", allergicTo("shellfish", 10))
test_true("allergic to everything", allergicTo("shellfish", 255))

test_false("not allergic to anything", allergicTo("strawberries", 0))
test_true("allergic only to strawberries", allergicTo("strawberries", 8))
test_true("allergic to strawberries and something else", allergicTo("strawberries", 28))
test_false("allergic to something, but not strawberries", allergicTo("strawberries", 20))
test_true("allergic to everything", allergicTo("strawberries", 255))

test_false("not allergic to anything", allergicTo("tomatoes", 0))
test_true("allergic only to tomatoes", allergicTo("tomatoes", 16))
test_true("allergic to tomatoes and something else", allergicTo("tomatoes", 56))
test_false("allergic to something, but not tomatoes", allergicTo("tomatoes", 40))
test_true("allergic to everything", allergicTo("tomatoes", 255))

test_false("not allergic to anything", allergicTo("chocolate", 0))
test_true("allergic only to chocolate", allergicTo("chocolate", 32))
test_true("allergic to chocolate and something else", allergicTo("chocolate", 112))
test_false("allergic to something, but not chocolate", allergicTo("chocolate", 80))
test_true("allergic to everything", allergicTo("chocolate", 255))

test_false("not allergic to anything", allergicTo("pollen", 0))
test_true("allergic only to pollen", allergicTo("pollen", 64))
test_true("allergic to pollen and something else", allergicTo("pollen", 224))
test_false("allergic to something, but not pollen", allergicTo("pollen", 160))
test_true("allergic to everything", allergicTo("pollen", 255))

test_false("not allergic to anything", allergicTo("cats", 0))
test_true("allergic only to cats", allergicTo("cats", 128))
test_true("allergic to cats and something else", allergicTo("cats", 192))
test_false("allergic to something, but not cats", allergicTo("cats", 64))
test_true("allergic to everything", allergicTo("cats", 255))

test_equal("no allergies", {}, list(0))
test_equal("just eggs", {"eggs"}, list(1))
test_equal("just peanuts", {"peanuts"}, list(2))
test_equal("just strawberries", {"strawberries"}, list(8))
test_equal("eggs and peanuts", {"eggs", "peanuts"}, list(3))
test_equal("more than eggs but not peanuts", {"eggs", "shellfish"}, list(5))
test_equal("lots of stuff", {
        "strawberries",
        "tomatoes",
        "chocolate",
        "pollen",
        "cats"
    }, list(248)
)
test_equal("everything", {
        "eggs",
        "peanuts",
        "shellfish",
        "strawberries",
        "tomatoes",
        "chocolate",
        "pollen",
        "cats"
    }, list(255)
)
test_equal("no allergen score parts", {
        "eggs",
        "shellfish",
        "strawberries",
        "tomatoes",
        "chocolate",
        "pollen",
        "cats"
    }, list(509)
)
test_equal("no allergen score parts without highest valid score", {"eggs"}, list(257))

test_report()
