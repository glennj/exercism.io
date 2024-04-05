include std/unittest.e

include luhn.ex

constant true = 1, false = 0

set_test_verbosity(TEST_SHOW_ALL)

test_equal("single digit strings can not be valid", false, valid("1"))
test_equal("a single zero is invalid", false, valid("0"))
test_equal("a simple valid SIN that remains valid if reversed", true, valid("059"))
test_equal("a simple valid SIN that becomes invalid if reversed", true, valid("59"))
test_equal("a valid Canadian SIN", true, valid("055 444 285"))
test_equal("invalid Canadian SIN", false, valid("055 444 286"))
test_equal("invalid credit card", false, valid("8273 1232 7352 0569"))
test_equal("invalid long number with an even remainder", false, valid("1 2345 6789 1234 5678 9012"))
test_equal("invalid long number with a remainder divisible by 5", false, valid("1 2345 6789 1234 5678 9013"))
test_equal("valid number with an even number of digits", true, valid("095 245 88"))
test_equal("valid number with an odd number of spaces", true, valid("234 567 891 234"))
test_equal("valid strings with a non-digit added at the end become invalid_luhn", false, valid("059a"))
test_equal("valid strings with punctuation included become invalid_luhn", false, valid("055-444-285"))
test_equal("valid strings with symbols included become invalid_luhn", false, valid("055# 444$ 285"))
test_equal("single zero with space is invalid", false, valid(" 0"))
test_equal("more than a single zero is valid", true, valid("0000 0"))
test_equal("input digit 9 is correctly converted to output digit 9", true, valid("091"))
test_equal("very long input is valid", true, valid("9999999999 9999999999 9999999999 9999999999"))
test_equal("valid luhn with an odd number of digits and non zero first digit", true, valid("109"))
test_equal("using ascii value for non-doubled non-digit isn't allowed", false, valid("055b 444 285"))
test_equal("using ascii value for doubled non-digit isn't allowed", false, valid(":9"))
test_equal("non-numeric,  non-space char in the middle with a sum that's divisible by 10 isn't allowed", false, valid("59%59"))

test_report()
