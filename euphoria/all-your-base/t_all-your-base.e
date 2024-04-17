include std/unittest.e 
include lib/errors.e

include all-your-base.ex 

set_test_verbosity(TEST_SHOW_ALL)

test_equal("single bit one to decimal", {OK, {1}}, rebase(2, {1}, 10))
test_equal("binary to single decimal", {OK, {5}}, rebase(2, {1, 0, 1}, 10))
test_equal("single decimal to binary", {OK, {1, 0, 1}}, rebase(10, {5}, 2))
test_equal("binary to multiple decimal", {OK, {4, 2}}, rebase(2, {1, 0, 1, 0, 1, 0}, 10))
test_equal("decimal to binary", {OK, {1, 0, 1, 0, 1, 0}}, rebase(10, {4, 2}, 2))
test_equal("trinary to hexadecimal", {OK, {2, 10}}, rebase(3, {1, 1, 2, 0}, 16))
test_equal("hexadecimal to trinary", {OK, {1, 1, 2, 0}}, rebase(16, {2, 10}, 3))
test_equal("15-bit integer", {OK, {6, 10, 45}}, rebase(97, {3, 46, 60}, 73))
test_equal("empty list", {OK, {0}}, rebase(2, {}, 10))
test_equal("single zero", {OK, {0}}, rebase(10, {0}, 2))
test_equal("multiple zeros", {OK, {0}}, rebase(10, {0, 0, 0}, 2))
test_equal("leading zeros", {OK, {4, 2}}, rebase(7, {0, 6, 0}, 10))

test_equal("input base is one", {ERR, "input base must be >= 2"}, rebase(1, {0}, 10))
test_equal("input base is zero", {ERR, "input base must be >= 2"}, rebase(0, {}, 10))
test_equal("input base is negative", {ERR, "input base must be >= 2"}, rebase(-2, {1}, 10))
test_equal("negative digit", {ERR, "all digits must satisfy 0 <= d < input base"}, rebase(2, {1, -1, 1, 0, 1, 0}, 10))
test_equal("invalid positive digit", {ERR, "all digits must satisfy 0 <= d < input base"}, rebase(2, {1, 2, 1, 0, 1, 0}, 10))
test_equal("output base is one", {ERR, "output base must be >= 2"}, rebase(2, {1, 0, 1, 0, 1, 0}, 1))
test_equal("output base is zero", {ERR, "output base must be >= 2"}, rebase(10, {7}, 0))
test_equal("output base is negative", {ERR, "output base must be >= 2"}, rebase(2, {1}, -7))
test_equal("both bases are negative", {ERR, "input base must be >= 2"}, rebase(-2, {1}, -7))


test_report() 
