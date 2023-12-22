use context essentials2020

include file("clock.arr")

#|
  When working offline, all tests except the first one are skipped by default.
  Once you get the first test running, unskip the next one until all tests pass locally.
  Check the block comment below for further details.
|#

fun create-clock-on-hour():
  check "Create a new clock with an initial time -> on the hour":
    clock(8, 0).normalize().to-string() is "08:00"
  end
end

fun create-clock-past-hour():
  check "Create a new clock with an initial time -> past the hour":
    clock(11, 09).normalize().to-string() is "11:09"
  end
end

fun create-clock-midnight():
  check "Create a new clock with an initial time -> midnight is zero hours":
    clock(24, 0).normalize().to-string() is "00:00"
  end
end

fun create-clock-hours-rollover():
  check "Create a new clock with an initial time -> hour rolls over":
    clock(25, 0).normalize().to-string() is "01:00"
  end
end

fun create-clock-hours-rollover-multiple():
  check "Create a new clock with an initial time -> hour rolls over continuously":
    clock(100, 0).normalize().to-string() is "04:00"
  end
end

fun create-clock-sixty-minutes():
  check "Create a new clock with an initial time -> sixty minutes is next hour":
    clock(1, 60).normalize().to-string() is "02:00"
  end
end

fun create-clock-minutes-rollover():
  check "Create a new clock with an initial time -> minutes roll over":
    clock(0, 160).normalize().to-string() is "02:40"
  end
end

fun create-clock-minutes-rollover-multiple():
  check "Create a new clock with an initial time -> minutes roll over continuously":
    clock(0, 1723).normalize().to-string() is "04:43"
  end
end

fun create-clock-rollover():
  check "Create a new clock with an initial time -> hour and minutes roll over":
    clock(25, 160).normalize().to-string() is "03:40"
  end
end

fun create-clock-rollover-multiple():
  check "Create a new clock with an initial time -> hour and minutes roll over continuously":
    clock(201, 3001).normalize().to-string() is "11:01"
  end
end

fun create-clock-rollover-to-midnight():
  check "Create a new clock with an initial time -> hour and minutes roll over to exactly midnight":
    clock(72, 8640).normalize().to-string() is "00:00"
  end
end

fun create-clock-negative-hours():
  check "Create a new clock with an initial time -> negative hour":
    clock(-1, 15).normalize().to-string() is "23:15"
  end
end

fun create-clock-negative-hours-rollover():
  check "Create a new clock with an initial time -> negative hour rolls over":
    clock(-25, 0).normalize().to-string() is "23:00"
  end
end

fun create-clock-negative-hours-rollover-multiple():
  check "Create a new clock with an initial time -> negative hour rolls over continuously":
    clock(-91, 0).normalize().to-string() is "05:00"
  end
end

fun create-clock-negative-minutes():
  check "Create a new clock with an initial time -> negative minutes":
    clock(1, -40).normalize().to-string() is "00:20"
  end
end

fun create-clock-negative-minutes-rollover():
  check "Create a new clock with an initial time -> negative minutes roll over":
    clock(1, -160).normalize().to-string() is "22:20"
  end
end

fun create-clock-negative-minutes-rollover-multiple():
  check "Create a new clock with an initial time -> negative minutes roll over continuously":
    clock(1, -4820).normalize().to-string() is "16:40"
  end
end

fun create-clock-negative-sixty-minutes():
  check "Create a new clock with an initial time -> negative sixty minutes is previous hour":
    clock(2, -60).normalize().to-string() is "01:00"
  end
end

fun create-clock-negative-rollover():
  check "Create a new clock with an initial time -> negative hour and minutes both roll over":
    clock(-25, -160).normalize().to-string() is "20:20"
  end
end

fun create-clock-negative-rollover-multiple():
  check "Create a new clock with an initial time -> negative hour and minutes both roll over continuously":
    clock(-121, -5810).normalize().to-string() is "22:10"
  end
end

fun add-minutes():
  check "Add minutes -> add minutes":
    clock(10, 0).add(3).to-string() is "10:03"
  end
end

fun add-no-minutes():
  check "Add minutes -> add no minutes":
    clock(6, 41).add(0).to-string() is "06:41"
  end
end

fun add-to-hours():
  check "Add minutes -> add to next hour":
    clock(0, 45).add(40).to-string() is "01:25"
  end
end

fun add-to-hours-multiple():
  check "Add minutes -> add more than one hour":
    clock(10, 0).add(61).to-string() is "11:01"
  end
end

fun add-to-hours-carryover():
  check "Add minutes -> add more than two hours with carry":
    clock(0, 45).add(160).to-string() is "03:25"
  end
end

fun add-across-midnight():
  check "Add minutes -> add across midnight":
    clock(23, 59).add(2).to-string() is "00:01"
  end
end

fun add-more-than-one-day():
  check "Add minutes -> add more than one day (1500 min = 25 hrs)":
    clock(5, 32).add(1500).to-string() is "06:32"
  end
end

fun add-more-than-one-day-multiple():
  check "Add minutes -> add more than two days":
    clock(1, 1).add(3500).to-string() is "11:21"
  end
end

fun subtract-minutes():
  check "Subtract minutes -> subtract minutes":
    clock(10, 3).subtract(3).to-string() is "10:00"
  end
end

fun subtract-rollover():
  check "Subtract minutes -> subtract to previous hour":
    clock(10, 3).subtract(30).to-string() is "09:33"
  end
end

fun subtract-rollover-multiple():
  check "Subtract minutes -> subtract more than an hour":
    clock(10, 3).subtract(70).to-string() is "08:53"
  end
end

fun subtract-across-midnight():
  check "Subtract minutes -> subtract across midnight":
    clock(0, 3).subtract(4).to-string() is "23:59"
  end
end

fun subtract-more-than-two-hours():
  check "Subtract minutes -> subtract more than two hours":
    clock(0, 0).subtract(160).to-string() is "21:20"
  end
end

fun subtract-rollover-borrow():
  check "Subtract minutes -> subtract more than two hours with borrow":
    clock(6, 15).subtract(160).to-string() is "03:35"
  end
end

fun subtract-more-than-a-day():
  check "Subtract minutes -> subtract more than one day (1500 min = 25 hrs)":
    clock(5, 32).subtract(1500).to-string() is "04:32"
  end
end

fun subtract-more-than-two-days():
  check "Subtract minutes -> subtract more than two days":
    clock(2, 20).subtract(3000).to-string() is "00:20"
  end
end

fun equality-same-time():
  check "Compare two clocks for equality -> clocks with same time":
    clock(15, 37) is clock(15, 37)
  end
end

fun equality-a-minute-apart():
  check "Compare two clocks for equality -> clocks a minute apart":
    clock(15, 36) is-not clock(15, 37)
  end
end

fun equality-an-hour-apart():
  check "Compare two clocks for equality -> clocks an hour apart":
    clock(14, 37) is-not clock(15, 37)
  end
end

fun equality-hour-rollover():
  check "Compare two clocks for equality -> clocks with hour overflow":
    clock(10, 37) is clock(34, 37)
  end
end

fun equality-hour-overflow-multiple():
  check "Compare two clocks for equality -> clocks with hour overflow by several days":
    clock(3, 11) is clock(99, 11)
  end
end

fun equality-negative-hour():
  check "Compare two clocks for equality -> clocks with negative hour":
    clock(22, 40) is clock(-2, 40)
  end
end

fun equality-negative-hour-rollover():
  check "Compare two clocks for equality -> clocks with negative hour that wraps":
    clock(17, 3) is clock(-31, 3)
  end
end

fun equality-negative-hour-rollover-multiple():
  check "Compare two clocks for equality -> clocks with negative hour that wraps multiple times":
    clock(13, 49) is clock(-83, 49)
  end
end

fun equality-minute-rollover():
  check "Compare two clocks for equality -> clocks with minute overflow":
    clock(0, 1) is clock(0, 1441)
  end
end

fun equality-rollover-multiple():
  check "Compare two clocks for equality -> clocks with minute overflow by several days":
    clock(2, 2) is clock(2, 4322)
  end
end

fun equality-negative-minute():
  check "Compare two clocks for equality -> clocks with negative minute":
    clock(2, 40) is clock(3, -20)
  end
end

fun equality-negative-minute-rollover():
  check "Compare two clocks for equality -> clocks with negative minute that wraps":
    clock(4, 10) is clock(5, -1490)
  end
end

fun equality-negative-minute-rollover-multiple():
  check "Compare two clocks for equality -> clocks with negative minute that wraps multiple times":
    clock(6, 15) is clock(6, -4305)
  end
end

fun equality-negative-hour-and-minute():
  check "Compare two clocks for equality -> clocks with negative hours and minutes":
    clock(7, 32) is clock(-12, -268)
  end
end

fun equality-negative-hours-and-minutes-rollover():
  check "Compare two clocks for equality -> clocks with negative hours and minutes that wrap":
    clock(18, 7) is clock(-54, -11513)
  end
end

fun equality-full-clock-empty-clock():
  check "Compare two clocks for equality -> full clock and zeroed clock":
    clock(24, 0) is clock(0, 0)
  end
end

#|
  Code to run each test. Each line corresponds to a test above and whether it should be run.
  To mark a test to be run, replace `false` with `true` on that same line after the comma.
  test(test-a, true) will be run. test(test-a, false) will be skipped.
|#

data TestRun: test(run, active) end

[list: 
  test(create-clock-on-hour, true),
  test(create-clock-past-hour, true),
  test(create-clock-midnight, true),
  test(create-clock-hours-rollover, true),
  test(create-clock-hours-rollover-multiple, true),
  test(create-clock-sixty-minutes, true),
  test(create-clock-minutes-rollover, true),
  test(create-clock-minutes-rollover-multiple, true),
  test(create-clock-rollover, true),
  test(create-clock-rollover-multiple, true),
  test(create-clock-rollover-to-midnight, true),
  test(create-clock-negative-hours, true),
  test(create-clock-negative-hours-rollover, true),
  test(create-clock-negative-hours-rollover-multiple, true),
  test(create-clock-negative-minutes, true),
  test(create-clock-negative-minutes-rollover-multiple, true),
  test(create-clock-negative-sixty-minutes, true),
  test(create-clock-negative-rollover, true),
  test(create-clock-negative-rollover-multiple, true),
  test(add-minutes, true),
  test(add-no-minutes, true),
  test(add-to-hours, true),
  test(add-to-hours-multiple, true),
  test(add-to-hours-carryover, true),
  test(add-across-midnight, true),
  test(add-more-than-one-day, true),
  test(add-more-than-one-day-multiple, true),
  test(subtract-minutes, true),
  test(subtract-rollover, true),
  test(subtract-rollover-multiple, true),
  test(subtract-across-midnight, true),
  test(subtract-more-than-two-hours, true),
  test(subtract-rollover-borrow, true),
  test(subtract-more-than-a-day, true),
  test(subtract-more-than-two-days, true),
  test(equality-same-time, true),
  test(equality-a-minute-apart, true),
  test(equality-an-hour-apart, true),
  test(equality-hour-rollover, true),
  test(equality-hour-overflow-multiple, true),
  test(equality-negative-hour, true),
  test(equality-negative-hour-rollover, true),
  test(equality-negative-hour-rollover-multiple, true),
  test(equality-minute-rollover, true),
  test(equality-rollover-multiple, true),
  test(equality-negative-minute, true),
  test(equality-negative-minute-rollover, true),
  test(equality-negative-minute-rollover-multiple, true),
  test(equality-negative-hour-and-minute, true),
  test(equality-negative-hours-and-minutes-rollover, true),
  test(equality-full-clock-empty-clock, true)
].each(lam(t): when t.active: t.run() end end)
