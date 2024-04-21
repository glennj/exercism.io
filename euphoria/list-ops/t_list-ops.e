include std/math.e
include std/unittest.e

include list-ops.ex

set_test_verbosity(TEST_SHOW_ALL)

test_equal("append entries to a sequence and return the new sequence - empty sequences",
           {}, my_append({}, {}))
test_equal("append entries to a sequence and return the new sequence - sequence to empty sequence",
           {1, 2, 3, 4}, my_append({}, {1, 2, 3, 4}))
test_equal("append entries to a sequence and return the new sequence - empty sequence to sequence",
           {1, 2, 3, 4}, my_append({1, 2, 3, 4}, {}))
test_equal("append entries to a sequence and return the new sequence - non-empty sequences",
           {1, 2, 2, 3, 4, 5}, my_append({1, 2}, {2, 3, 4, 5}))

test_equal("concatenate a sequence of sequences - empty sequence",
           {}, my_concatenate({}))
test_equal("concatenate a sequence of sequences - sequence of sequences",
           {1, 2, 3, 4, 5, 6},
           my_concatenate({{1, 2}, {3}, {}, {4, 5, 6}}))
test_equal("concatenate a sequence of sequences - sequence of nested sequences",
           {{1}, {2}, {3}, {}, {4, 5, 6}},
           my_concatenate({{{1}, {2}}, {{3}}, {{}}, {{4, 5, 6}}}))

test_equal("filter sequence returning only values that satisfy the filter function - empty sequence",
           {}, my_filter({}, routine_id("is_odd")))
test_equal("filter sequence returning only values that satisfy the filter function - non-empty sequence",
           {1, 3, 5}, my_filter({1, 2, 3, 5}, routine_id("is_odd")))

test_equal("returns the length of a sequence - empty sequence",
           0, my_length({}))
test_equal("returns the length of a sequence - non-empty sequence",
           4, my_length({1, 2, 3, 4}))

test_equal("return a sequence of elements whose values equal the sequence value transformed by the mapping function - empty sequence",
           {}, my_map({}, routine_id("add_one")))
test_equal("return a sequence of elements whose values equal the sequence value transformed by the mapping function - non-empty sequence",
           {2, 4, 6, 8}, my_map({1, 3, 5, 7}, routine_id("add_one")))

test_equal("folds (reduces) the given sequence from the left with a function - empty sequence",
           2, my_foldl({}, routine_id("multiply"), 2))
test_equal("folds (reduces) the given sequence from the left with a function -> direction independent function applied to non-empty sequence",
           15, my_foldl({1, 2, 3, 4}, routine_id("add"), 5))
test_equal("folds (reduces) the given sequence from the left with a function -> direction dependent function applied to non-empty sequence",
           0, my_foldl({2, 5}, routine_id("divide"), 5))

test_equal("folds (reduces) the given sequence from the right with a function - empty sequence",
           2, my_foldr({}, routine_id("multiply"), 2))
test_equal("folds (reduces) the given sequence from the right with a function -> direction independent function applied to non-empty sequence",
           15, my_foldr({1, 2, 3, 4}, routine_id("add"), 5))
test_equal("folds (reduces) the given sequence from the right with a function -> direction dependent function applied to non-empty sequence",
           2, my_foldr({2, 5}, routine_id("divide"), 5))

test_equal("reverse the elements of the sequence - empty sequence",
           {}, my_reverse({}))
test_equal("reverse the elements of the sequence - non-empty sequence",
           {7, 5, 3, 1}, my_reverse({1, 3, 5, 7}))
test_equal("reverse the elements of the sequence - sequence of sequences is not flattened",
           {{4, 5, 6}, {}, {3}, {1, 2}},
           my_reverse({{1, 2}, {3}, {}, {4, 5, 6}}))

test_report()

function is_odd(integer num)
  return not is_even(num)
end function

function add_one(integer num)
  return num + 1
end function

function add(integer n1, integer n2)
  return n1 + n2
end function

function multiply(integer n1, integer n2)
  return n1 * n2
end function

function divide(integer n1, integer n2)
  return trunc(n1 / n2)
end function
