use context essentials2020

include file("circular-buffer.arr")

#|
  When working offline, all tests except the first one are skipped by default.
  Once you get the first test running, unskip the next one until all tests pass locally.
  Check the block comment below for further details.
|#

fun read-empty-buffer-should-fail():
  check "reading empty buffer should fail":
    buff = make-buffer(1)

    buff.read() raises "empty buffer"
  end
end

fun can-read-an-item-just-written():
  check "can read an item just written":
    var buff = make-buffer(1)
    buff := buff.write(1)

    buff.read().{0} is 1
  end
end

fun each-item-may-only-be-read-once():
  check "each item may only be read once":
    var buff = make-buffer(1)
    buff := buff.write(1)

    results = buff.read()
    results.{0} is 1
    results.{1}.read() raises "empty buffer"
  end
end

fun items-are-read-in-the-order-they-are-written():
  check "items are read in the order they are written":
    var buff = make-buffer(2)
    buff := buff.write(1)
    buff := buff.write(2)

    var results = buff.read()
    results.{0} is 1
    results := results.{1}.read()
    results.{0} is 2
  end
end

fun full-buffer-can-not-be-written-to():
  check "full buffer can't be written to":
    var buff = make-buffer(1)
    buff := buff.write(1)

    buff.write(2) raises "full buffer"
  end
end

fun a-read-frees-up-capacity-for-another-write():
  check "a read frees up capacity for another write":
    var buff = make-buffer(1)
    buff := buff.write(1)

    var results = buff.read()
    results.{0} is 1
  
    buff := results.{1}.write(2)
    results := buff.read()
    results.{0} is 2
  end
end

fun read-position-is-maintained-even-across-multiple-writes():
  check "read position is maintained even across multiple writes":
    var buff = make-buffer(3)
    buff := buff.write(1)
    buff := buff.write(2)

    var results = buff.read()
    results.{0} is 1

    buff := results.{1}
    buff := buff.write(3)
  
    results := buff.read()
    results.{0} is 2

    buff := results.{1}
    results := buff.read()
    results.{0} is 3
  end
end

fun items-cleared-out-of-buffer-can-not-be-read():
  check "items cleared out of buffer can't be read":
    var buff = make-buffer(1)
    buff := buff.write(1)
    buff := buff.clear()
    
    buff.read() raises "empty buffer"
  end
end

fun clear-frees-up-capacity-for-another-write():
  check "clear frees up capacity for another write":
    var buff = make-buffer(1)
    buff := buff.write(1)
    buff := buff.clear()
    buff := buff.write(2)

    buff.read().{0} is 2
  end
end

fun clear-does-nothing-on-empty-buffer():
  check "clear does nothing on empty buffer":
    var buff = make-buffer(1)
    buff := buff.clear()
    buff := buff.write(1)
  
    results = buff.read()
    results.{0} is 1
  end
end

fun overwrite-acts-like-write-on-non-full-buffer():
  check "overwrite acts like write on non-full buffer":
    var buff = make-buffer(2)
    buff := buff.write(1)
    buff := buff.overwrite(2)

    var results = buff.read()
    results.{0} is 1

    buff := results.{1}
    results := buff.read()
    results.{0} is 2
  end
end

fun overwrite-replaces-the-oldest-item-on-full-buffer():
  check "overwrite replaces the oldest item on full buffer":
    var buff = make-buffer(2)
    buff := buff.write(1)
    buff := buff.write(2)
    buff := buff.overwrite(3)

    var results = buff.read()
    results.{0} is 2

    buff := results.{1}
    results := buff.read()
    results.{0} is 3
  end
end

fun overwrite-replaces-the-oldest-item-remaining-in-buffer-following-a-read():
  check "overwrite replaces the oldest item remaining in buffer following a read":
    var buff = make-buffer(3)
    buff := buff.write(1)
    buff := buff.write(2)
    buff := buff.write(3)

    var results = buff.read()
    results.{0} is 1

    buff := results.{1}
    buff := buff.write(4)
    buff := buff.overwrite(5)
  
    results := buff.read()
    results.{0} is 3

    buff := results.{1}
    results := buff.read()
    results.{0} is 4

    buff := results.{1}
    results := buff.read()
    results.{0} is 5
  end
end

fun initial-clear-does-not-affect-wrapping-around():
  check "initial clear does not affect wrapping around":
    var buff = make-buffer(2)
    buff := buff.clear()
    buff := buff.write(1)
    buff := buff.write(2)
    buff := buff.overwrite(3)
    buff := buff.overwrite(4)

    var results = buff.read()
    results.{0} is 3

    buff := results.{1}
    results := buff.read()
    results.{0} is 4

    buff := results.{1}
    buff.read() raises "empty buffer"
  end
end

#|
  Code to run each test. Each line corresponds to a test above and whether it should be run.
  To mark a test to be run, replace `false` with `true` on that same line after the comma.
  test(test-a, true) will be run. test(test-a, false) will be skipped.
|#

data TestRun: test(run, active) end

[list:
  test(read-empty-buffer-should-fail, true),
  test(can-read-an-item-just-written, true),
  test(each-item-may-only-be-read-once, true),
  test(items-are-read-in-the-order-they-are-written, true),
  test(full-buffer-can-not-be-written-to, true),
  test(a-read-frees-up-capacity-for-another-write, true),
  test(read-position-is-maintained-even-across-multiple-writes, true),
  test(items-cleared-out-of-buffer-can-not-be-read, true),
  test(clear-frees-up-capacity-for-another-write, true),
  test(clear-does-nothing-on-empty-buffer, true),
  test(overwrite-acts-like-write-on-non-full-buffer, true),
  test(overwrite-replaces-the-oldest-item-on-full-buffer, true),
  test(overwrite-replaces-the-oldest-item-remaining-in-buffer-following-a-read, true),
  test(initial-clear-does-not-affect-wrapping-around, true)
].each(lam(t): when t.active: t.run() end end)
