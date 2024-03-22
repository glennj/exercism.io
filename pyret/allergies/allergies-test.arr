use context essentials2020

include file("allergies.arr")

#|
  When working offline, all tests except the first one are skipped by default.
  Once you get the first test running, unskip the next one until all tests pass locally.
  Check the block comment below for further details.
|#

# Eggs

fun eggs-allergy-not-allergic-to-anything():
  check "Eggs allergy - not allergic to anything":
    allergens = allergies(0)
    allergens.allergicTo("eggs") is false
  end
end

fun eggs-allergy-allergic-only-to-eggs():
  check "Eggs allergy - allergic only to eggs":
    allergens = allergies(1)
    allergens.allergicTo("eggs") is true
  end
end

fun eggs-allergy-allergic-to-eggs-and-something-else():
  check "Eggs allergy - allergic to eggs and something else":
    allergens = allergies(3)
    allergens.allergicTo("eggs") is true
  end
end

fun eggs-allergy-allergic-to-something-but-not-eggs():
  check "Eggs allergy - allergic to something, but not eggs":
    allergens = allergies(2)
    allergens.allergicTo("eggs") is false
  end
end

fun eggs-allergy-allergic-to-everything():
  check "Eggs allergy - allergic to everything":
    allergens = allergies(255)
    allergens.allergicTo("eggs") is true
  end
end

# Peanuts

fun peanuts-allergy-not-allergic-to-anything():
  check "peanuts allergy - not allergic to anything":
    allergens = allergies(0)
    allergens.allergicTo("peanuts") is false
  end
end

fun peanuts-allergy-allergic-only-to-peanuts():
  check "peanuts allergy - allergic only to peanuts":
    allergens = allergies(2)
    allergens.allergicTo("peanuts") is true
  end
end

fun peanuts-allergy-allergic-to-peanuts-and-something-else():
  check "peanuts allergy - allergic to peanuts and something else":
    allergens = allergies(7)
    allergens.allergicTo("peanuts") is true
  end
end

fun peanuts-allergy-allergic-to-something-but-not-peanuts():
  check "peanuts allergy - allergic to something, but not peanuts":
    allergens = allergies(5)
    allergens.allergicTo("peanuts") is false
  end
end

fun peanuts-allergy-allergic-to-everything():
  check "peanuts allergy - allergic to everything":
    allergens = allergies(255)
    allergens.allergicTo("peanuts") is true
  end
end

# Shellfish

fun shellfish-allergy-not-allergic-to-anything():
  check "shellfish allergy - not allergic to anything":
    allergens = allergies(0)
    allergens.allergicTo("shellfish") is false
  end
end

fun shellfish-allergy-allergic-only-to-shellfish():
  check "shellfish allergy - allergic only to shellfish":
    allergens = allergies(4)
    allergens.allergicTo("shellfish") is true
  end
end

fun shellfish-allergy-allergic-to-shellfish-and-something-else():
  check "shellfish allergy - allergic to shellfish and something else":
    allergens = allergies(14)
    allergens.allergicTo("shellfish") is true
  end
end

fun shellfish-allergy-allergic-to-something-but-not-shellfish():
  check "shellfish allergy - allergic to something, but not shellfish":
    allergens = allergies(10)
    allergens.allergicTo("shellfish") is false
  end
end

fun shellfish-allergy-allergic-to-everything():
  check "shellfish allergy - allergic to everything":
    allergens = allergies(255)
    allergens.allergicTo("shellfish") is true
  end
end

# Strawberries

fun strawberries-allergy-not-allergic-to-anything():
  check "strawberries allergy - not allergic to anything":
    allergens = allergies(0)
    allergens.allergicTo("strawberries") is false
  end
end

fun strawberries-allergy-allergic-only-to-strawberries():
  check "strawberries allergy - allergic only to strawberries":
    allergens = allergies(8)
    allergens.allergicTo("strawberries") is true
  end
end

fun strawberries-allergy-allergic-to-strawberries-and-something-else():
  check "strawberries allergy - allergic to strawberries and something else":
    allergens = allergies(28)
    allergens.allergicTo("strawberries") is true
  end
end

fun strawberries-allergy-allergic-to-something-but-not-strawberries():
  check "strawberries allergy - allergic to something, but not strawberries":
    allergens = allergies(20)
    allergens.allergicTo("strawberries") is false
  end
end

fun strawberries-allergy-allergic-to-everything():
  check "strawberries allergy - allergic to everything":
    allergens = allergies(255)
    allergens.allergicTo("strawberries") is true
  end
end

# Tomatoes

fun tomatoes-allergy-not-allergic-to-anything():
  check "tomatoes allergy - not allergic to anything":
    allergens = allergies(0)
    allergens.allergicTo("tomatoes") is false
  end
end

fun tomatoes-allergy-allergic-only-to-tomatoes():
  check "tomatoes allergy - allergic only to tomatoes":
    allergens = allergies(16)
    allergens.allergicTo("tomatoes") is true
  end
end

fun tomatoes-allergy-allergic-to-tomatoes-and-something-else():
  check "tomatoes allergy - allergic to tomatoes and something else":
    allergens = allergies(56)
    allergens.allergicTo("tomatoes") is true
  end
end

fun tomatoes-allergy-allergic-to-something-but-not-tomatoes():
  check "tomatoes allergy - allergic to something, but not tomatoes":
    allergens = allergies(40)
    allergens.allergicTo("tomatoes") is false
  end
end

fun tomatoes-allergy-allergic-to-everything():
  check "tomatoes allergy - allergic to everything":
    allergens = allergies(255)
    allergens.allergicTo("tomatoes") is true
  end
end

# Chocolate

fun chocolate-allergy-not-allergic-to-anything():
  check "chocolate allergy - not allergic to anything":
    allergens = allergies(0)
    allergens.allergicTo("chocolate") is false
  end
end

fun chocolate-allergy-allergic-only-to-chocolate():
  check "chocolate allergy - allergic only to chocolate":
    allergens = allergies(32)
    allergens.allergicTo("chocolate") is true
  end
end

fun chocolate-allergy-allergic-to-chocolate-and-something-else():
  check "chocolate allergy - allergic to chocolate and something else":
    allergens = allergies(112)
    allergens.allergicTo("chocolate") is true
  end
end

fun chocolate-allergy-allergic-to-something-but-not-chocolate():
  check "chocolate allergy - allergic to something, but not chocolate":
    allergens = allergies(80)
    allergens.allergicTo("chocolate") is false
  end
end

fun chocolate-allergy-allergic-to-everything():
  check "chocolate allergy - allergic to everything":
    allergens = allergies(255)
    allergens.allergicTo("chocolate") is true
  end
end

# Pollen

fun pollen-allergy-not-allergic-to-anything():
  check "pollen allergy - not allergic to anything":
    allergens = allergies(0)
    allergens.allergicTo("pollen") is false
  end
end

fun pollen-allergy-allergic-only-to-pollen():
  check "pollen allergy - allergic only to pollen":
    allergens = allergies(64)
    allergens.allergicTo("pollen") is true
  end
end

fun pollen-allergy-allergic-to-pollen-and-something-else():
  check "pollen allergy - allergic to pollen and something else":
    allergens = allergies(224)
    allergens.allergicTo("pollen") is true
  end
end

fun pollen-allergy-allergic-to-something-but-not-pollen():
  check "pollen allergy - allergic to something, but not pollen":
    allergens = allergies(160)
    allergens.allergicTo("pollen") is false
  end
end

fun pollen-allergy-allergic-to-everything():
  check "pollen allergy - allergic to everything":
    allergens = allergies(255)
    allergens.allergicTo("pollen") is true
  end
end

# Cats

fun cats-allergy-not-allergic-to-anything():
  check "cats allergy - not allergic to anything":
    allergens = allergies(0)
    allergens.allergicTo("cats") is false
  end
end

fun cats-allergy-allergic-only-to-cats():
  check "cats allergy - allergic only to cats":
    allergens = allergies(128)
    allergens.allergicTo("cats") is true
  end
end

fun cats-allergy-allergic-to-cats-and-something-else():
  check "cats allergy - allergic to cats and something else":
    allergens = allergies(192)
    allergens.allergicTo("cats") is true
  end
end

fun cats-allergy-allergic-to-something-but-not-cats():
  check "cats allergy - allergic to something, but not cats":
    allergens = allergies(64)
    allergens.allergicTo("cats") is false
  end
end

fun cats-allergy-allergic-to-everything():
  check "cats allergy - allergic to everything":
    allergens = allergies(255)
    allergens.allergicTo("cats") is true
  end
end

# List

fun list-when-no-allergies():
  check "list when no allergies":
    allergens = allergies(0)
    allergens.list() is [list: ]
  end
end

fun list-when-just-eggs():
  check "list when just eggs":
    allergens = allergies(1)
    allergens.list() is [list: "eggs"]
  end
end

fun list-when-just-peanuts():
  check "list when just peanuts":
    allergens = allergies(2)
    allergens.list() is [list: "peanuts"]
  end
end

fun list-when-just-strawberries():
  check "list when just strawberries":
    allergens = allergies(8)
    allergens.list() is [list: "strawberries"]
  end
end

fun list-when-eggs-and-peanuts():
  check "list when eggs and peanuts":
    allergens = allergies(3)
    allergens.list() is [list: "eggs", "peanuts"]
  end
end

fun list-when-more-than-eggs-but-not-peanuts():
  check "list when more than eggs but not peanuts":
    allergens = allergies(5)
    allergens.list() is [list: "eggs", "shellfish"]
  end
end

fun list-when-lots-of-stuff():
  check "list when lots of stuff":
    allergens = allergies(248)
    allergens.list() is [list: 
        "strawberries",
        "tomatoes",
        "chocolate",
        "pollen",
        "cats"]
  end
end

fun list-when-everything():
  check "list when everything":
    allergens = allergies(255)
    allergens.list() is [list: 
        "eggs",
        "peanuts",
        "shellfish",
        "strawberries",
        "tomatoes",
        "chocolate",
        "pollen",
        "cats"]
  end
end

fun list-when-no-allergen-score-parts():
  check "list when no allergen score parts":
    allergens = allergies(509)
    allergens.list() is [list: 
        "eggs", 
        "shellfish", 
        "strawberries", 
        "tomatoes", 
        "chocolate", 
        "pollen", 
        "cats"]
  end
end

fun list-when-no-allergen-score-parts-without-highest-valid-score():
  check "list when no allergen score parts without highest valid score":
    allergens = allergies(257)
    allergens.list() is [list: "eggs"]
  end
end

#|
  Code to run each test. Each line corresponds to a test above and whether it should be run.
  To mark a test to be run, replace `false` with `true` on that same line after the comma.
  test(test-a, true) will be run. test(test-a, false) will be skipped.
|#

data TestRun: test(run, active) end

[list:
  test(eggs-allergy-not-allergic-to-anything, true),
  test(eggs-allergy-allergic-only-to-eggs, true),
  test(eggs-allergy-allergic-to-eggs-and-something-else, true),
  test(eggs-allergy-allergic-to-something-but-not-eggs, true),
  test(eggs-allergy-allergic-to-everything, true),
  test(peanuts-allergy-not-allergic-to-anything, true),
  test(peanuts-allergy-allergic-only-to-peanuts, true),
  test(peanuts-allergy-allergic-to-peanuts-and-something-else, true),
  test(peanuts-allergy-allergic-to-something-but-not-peanuts, true),
  test(peanuts-allergy-allergic-to-everything, true),
  test(shellfish-allergy-not-allergic-to-anything, true),
  test(shellfish-allergy-allergic-only-to-shellfish, true),
  test(shellfish-allergy-allergic-to-shellfish-and-something-else, true),
  test(shellfish-allergy-allergic-to-something-but-not-shellfish, true),
  test(shellfish-allergy-allergic-to-everything, true),
  test(strawberries-allergy-not-allergic-to-anything, true),
  test(strawberries-allergy-allergic-only-to-strawberries, true),
  test(strawberries-allergy-allergic-to-strawberries-and-something-else, true),
  test(strawberries-allergy-allergic-to-something-but-not-strawberries, true),
  test(strawberries-allergy-allergic-to-everything, true),
  test(tomatoes-allergy-not-allergic-to-anything, true),
  test(tomatoes-allergy-allergic-only-to-tomatoes, true),
  test(tomatoes-allergy-allergic-to-tomatoes-and-something-else, true),
  test(tomatoes-allergy-allergic-to-something-but-not-tomatoes, true),
  test(tomatoes-allergy-allergic-to-everything, true),
  test(chocolate-allergy-not-allergic-to-anything, true),
  test(chocolate-allergy-allergic-only-to-chocolate, true),
  test(chocolate-allergy-allergic-to-chocolate-and-something-else, true),
  test(chocolate-allergy-allergic-to-something-but-not-chocolate, true),
  test(chocolate-allergy-allergic-to-everything, true),
  test(pollen-allergy-not-allergic-to-anything, true),
  test(pollen-allergy-allergic-only-to-pollen, true),
  test(pollen-allergy-allergic-to-pollen-and-something-else, true),
  test(pollen-allergy-allergic-to-something-but-not-pollen, true),
  test(pollen-allergy-allergic-to-everything, true),
  test(cats-allergy-not-allergic-to-anything, true),
  test(cats-allergy-allergic-only-to-cats, true),
  test(cats-allergy-allergic-to-cats-and-something-else, true),
  test(cats-allergy-allergic-to-something-but-not-cats, true),
  test(cats-allergy-allergic-to-everything, true),
  test(list-when-no-allergies, true),
  test(list-when-just-eggs, true),
  test(list-when-just-peanuts, true),
  test(list-when-just-strawberries, true),
  test(list-when-eggs-and-peanuts, true),
  test(list-when-more-than-eggs-but-not-peanuts, true),
  test(list-when-lots-of-stuff, true),
  test(list-when-everything, true),
  test(list-when-no-allergen-score-parts, true),
  test(list-when-no-allergen-score-parts-without-highest-valid-score, true),
].each(lam(t): when t.active: t.run() end end)
