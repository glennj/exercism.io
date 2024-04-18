include std/unittest.e

include luhn.ex

set_test_verbosity(TEST_SHOW_ALL)

test_equal("single digit strings can not be valid", 0, valid("1"))
test_equal("a single zero is invalid", 0, valid("0"))
test_equal("a simple valid SIN that remains valid if reversed", 1, valid("059"))
test_equal("a simple valid SIN that becomes invalid if reversed", 1, valid("59"))
test_equal("a valid Canadian SIN", 1, valid("055 444 285"))
test_equal("invalid Canadian SIN", 0, valid("055 444 286"))
test_equal("invalid credit card", 0, valid("8273 1232 7352 0569"))
test_equal("invalid long number with an even remainder", 0, valid("1 2345 6789 1234 5678 9012"))
test_equal("invalid long number with a remainder divisible by 5", 0, valid("1 2345 6789 1234 5678 9013"))
test_equal("valid number with an even number of digits", 1, valid("095 245 88"))
test_equal("valid number with an odd number of spaces", 1, valid("234 567 891 234"))
test_equal("valid strings with a non-digit added at the end become invalid_luhn", 0, valid("059a"))
test_equal("valid strings with punctuation included become invalid_luhn", 0, valid("055-444-285"))
test_equal("valid strings with symbols included become invalid_luhn", 0, valid("055# 444$ 285"))
test_equal("single zero with space is invalid", 0, valid(" 0"))
test_equal("more than a single zero is valid", 1, valid("0000 0"))
test_equal("input digit 9 is correctly converted to output digit 9", 1, valid("091"))
test_equal("very long input is valid", 1, valid("9999999999 9999999999 9999999999 9999999999"))
test_equal("valid luhn with an odd number of digits and non zero first digit", 1, valid("109"))
test_equal("using ascii value for non-doubled non-digit isn't allowed", 0, valid("055b 444 285"))
test_equal("using ascii value for doubled non-digit isn't allowed", 0, valid(":9"))
test_equal("non-numeric, non-space char in the middle with a sum that's divisible by 10 isn't allowed", 0, valid("59%59"))

test_report()