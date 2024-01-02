use context essentials2020

include file("simple-linked-list.arr")

# No canonical data available for this exercise so these have been ported from Python and F#

#|
  When working offline, all tests except the first one are skipped by default.
  Once you get the first test running, unskip the next one until all tests pass locally.
  Check the block comment below for further details.
|#

fun empty-list-length-zero():
  check "empty list has length zero":
    empty-list.length() is 0
  end
end

fun singleton-has-length-one():
  check "singleton list has length one":
    linked-list(1, empty-list).length() is 1
  end
end

fun non-empty-list-has-length():
  check "non-empty list has correct length":
    result = empty-list
      ^ linked-list(3, _)
      ^ linked-list(2, _)
      ^ linked-list(1, _)

    result.length() is 3
  end
end

fun singleton-has-head():
  check "singleton list has head":
    linked-list(1, empty-list).get-head() is 1
  end
end

fun non-empty-list-has-head():
  check "non-empty list has correct head":
    result = empty-list
      ^ linked-list(1, _)
      ^ linked-list(2, _)

    result.get-head() is 2
  end
end

fun push-to-non-empty-list():
  check "can push to non-empty list":
    result = empty-list
      ^ linked-list(3, _)
      ^ linked-list(2, _)
      ^ linked-list(1, _)
    
    result.push(4).length() is 4
  end
end

fun push-changes-empty-list-head():
  check "pushing to empty list changes head":
    result = empty-list.push(5)
    
    result.length() is 1
    result.get-head() is 5
  end
end

fun access-second-element():
  check "can access second element in list":
    result = empty-list
      ^ linked-list(3, _)
      ^ linked-list(4, _)
      ^ linked-list(5, _)

    result.get-tail().get-head() is 4
  end
end

fun singleton-has-no-tail():
  check "test singleton list head has no tail":
    linked-list(1, empty-list).get-tail() is empty-list
  end
end

fun non-empty-list-traverse():
  check "non-empty list traverse":
    my-list = range(0, 11).foldl(
      lam(elt, acc):
      acc.push(elt)
      end,
      empty-list)
    
    traversed = range(-10, 0).foldl(
      lam(n, acc) block:
        acc.get-head() is num-abs(n)
        acc.get-tail()
      end,
      my-list)

    traversed.get-tail() is empty-list
  end
end

fun empty-linked-list-is-empty-list():
  check "empty linked list to list is empty":
    empty-list.to-list() is [list: ]
  end
end

fun singleton-is-list-with-single-element():
  check "singleton linked list to list with single element":
    linked-list(1, empty-list).to-list() is [list: 1]
  end
end

fun non-empty-list-is-list-with-all-elements():
  check "non-empty linked list to list is list with all elements":
    result = empty-list
      ^ linked-list(3, _)
      ^ linked-list(2, _)
      ^ linked-list(1, _)

      result.to-list() is [list: 3, 2, 1]
  end
end

fun reversed-empty-list-is-empty-list():
  check "reversed empty list is empty list":
    empty-list.reversed().to-list() is [list: ]
  end
end

fun reversed-singleton-list-is-same-list():
  check "reversed singleton list is same list":
    linked-list(1, empty-list).reversed() is linked-list(1, empty-list)
  end
end

fun reverse-non-empty-list():
  check "reverse non-empty list":
    result = empty-list
      ^ linked-list(3, _)
      ^ linked-list(2, _)
      ^ linked-list(1, _)

    result.reversed().to-list() is [list: 1, 2, 3]
  end
end

#|
  Code to run each test. Each line corresponds to a test above and whether it should be run.
  To mark a test to be run, replace `false` with `true` on that same line after the comma.
  test(test-a, true) will be run. test(test-a, false) will be skipped.
|#

data TestRun: test(run, active) end

[list: 
  test(empty-list-length-zero, true),
  test(singleton-has-length-one, true),
  test(non-empty-list-has-length, true),
  test(singleton-has-head, true),
  test(non-empty-list-has-head, true),
  test(push-to-non-empty-list, true),
  test(push-changes-empty-list-head, true),
  test(access-second-element, true),
  test(singleton-has-no-tail, true),
  test(non-empty-list-traverse, true),
  test(empty-linked-list-is-empty-list, true),
  test(singleton-is-list-with-single-element, true),
  test(non-empty-list-is-list-with-all-elements, true),
  test(reversed-empty-list-is-empty-list, true),
  test(reversed-singleton-list-is-same-list, true),
  test(reverse-non-empty-list, true)
].each(lam(t): when t.active: t.run() end end)
