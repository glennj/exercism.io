include std/unittest.e

include luhn.ex

set_test_verbosity(TEST_SHOW_ALL)

test_equal("single digit strings can not be valid",valid("1"),0)
test_equal("a single zero is invalid",valid("0"),0)
test_equal("a simple valid SIN that remains valid if reversed",valid("059"),1)
test_equal("a simple valid SIN that becomes invalid if reversed",valid("59"),1)
test_equal("a valid Canadian SIN",valid("055 444 285"),1)
test_equal("invalid Canadian SIN",valid("055 444 286"),0)
test_equal("invalid credit card",valid("8273 1232 7352 0569"),0)
test_equal("invalid long number with an even remainder",valid("1 2345 6789 1234 5678 9012"),0)
test_equal("invalid long number with a remainder divisible by 5",valid("1 2345 6789 1234 5678 9013"),0)
test_equal("valid number with an even number of digits",valid("095 245 88"),1)
test_equal("valid number with an odd number of spaces",valid("234 567 891 234"),1)
test_equal("valid strings with a non-digit added at the end become invalid_luhn",valid("059a"),0)
test_equal("valid strings with punctuation included become invalid_luhn",valid("055-444-285"),0)
test_equal("valid strings with symbols included become invalid_luhn",valid("055# 444$ 285"),0)
test_equal("single zero with space is invalid",valid(" 0"),0)
test_equal("more than a single zero is valid",valid("0000 0"),1)
test_equal("input digit 9 is correctly converted to output digit 9",valid("091"),1)
test_equal("very long input is valid",valid("9999999999 9999999999 9999999999 9999999999"),1)
test_equal("valid luhn with an odd number of digits and non zero first digit",valid("109"),1)
test_equal("using ascii value for non-doubled non-digit isn't allowed",valid("055b 444 285"),0)
test_equal("using ascii value for doubled non-digit isn't allowed",valid(":9"),0)
test_equal("non-numeric, non-space char in the middle with a sum that's divisible by 10 isn't allowed",valid("59%59"),0)

test_report()