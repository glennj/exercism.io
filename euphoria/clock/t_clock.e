include std/unittest.e

include clock.ex

set_test_verbosity(TEST_SHOW_ALL)

test_equal("Create a new clock with an initial time - on the hour",
            "08:00",
            toString(create(8, 0)))
test_equal("Create a new clock with an initial time - past the hour",
            "11:09",
            toString(create(11, 9)))
test_equal("Create a new clock with an initial time - midnight is zero hours",
            "00:00",
            toString(create(24, 0)))
test_equal("Create a new clock with an initial time - hour rolls over",
            "01:00",
            toString(create(25, 0)))
test_equal("Create a new clock with an initial time - hour rolls over continuously",
            "04:00",
            toString(create(100, 0)))
test_equal("Create a new clock with an initial time - sixty minutes is next hour",
            "02:00",
            toString(create(1, 60)))
test_equal("Create a new clock with an initial time - minutes roll over",
            "02:40",
            toString(create(0, 160)))
test_equal("Create a new clock with an initial time - minutes roll over continuously",
            "04:43",
            toString(create(0, 1723)))
test_equal("Create a new clock with an initial time - hour and minutes roll over",
            "03:40",
            toString(create(25, 160)))
test_equal("Create a new clock with an initial time - hour and minutes roll over continuously",
            "11:01",
            toString(create(201, 3001)))
test_equal("Create a new clock with an initial time - hour and minutes roll over to exactly midnight",
            "00:00",
            toString(create(72, 8640)))
test_equal("Create a new clock with an initial time - negative hour",
            "23:15",
            toString(create(-1, 15)))
test_equal("Create a new clock with an initial time - negative hour rolls over",
            "23:00",
            toString(create(-25, 0)))
test_equal("Create a new clock with an initial time - negative hour rolls over continuously",
            "05:00",
            toString(create(-91, 0)))
test_equal("Create a new clock with an initial time - negative minutes",
            "00:20",
            toString(create(1, -40)))
test_equal("Create a new clock with an initial time - negative minutes roll over",
            "22:20",
            toString(create(1, -160)))
test_equal("Create a new clock with an initial time - negative sixty minutes is previous hour",
            "01:00",
            toString(create(2, -60)))
test_equal("Create a new clock with an initial time - negative hour and minutes both roll over",
            "20:20",
            toString(create(-25, -160)))
test_equal("Create a new clock with an initial time - negative hour and minutes both roll over continuously",
            "22:10",
            toString(create(-121, -5810)))

test_equal("Add minutes - add minutes",
            "10:03",
            toString(add(create(10, 0), 3)))
test_equal("Add minutes - add no minutes",
            "06:41",
            toString(add(create(6, 41), 0)))
test_equal("Add minutes - add to next hour",
            "01:25",
            toString(add(create(0, 45), 40)))
test_equal("Add minutes - add more than one hour",
            "11:01",
            toString(add(create(10, 0), 61)))
test_equal("Add minutes - add more than two hours with carry",
            "03:25",
            toString(add(create(0, 45), 160)))
test_equal("Add minutes - add across midnight",
            "00:01",
            toString(add(create(23, 59), 2)))
test_equal("Add minutes - add more than one day (1500 min = 25 hrs)",
            "06:32",
            toString(add(create(5, 32), 1500)))
test_equal("Add minutes - add more than two days",
            "11:21",
            toString(add(create(1, 1), 3500)))


test_equal("Subtract minutes - subtract minutes",
            "10:00",
            toString(subtract(create(10, 3), 3)))
test_equal("Subtract minutes - subtract to previous hour",
            "09:33",
            toString(subtract(create(10, 3), 30)))
test_equal("Subtract minutes - subtract more than an hour",
            "08:53",
            toString(subtract(create(10, 3), 70)))
test_equal("Subtract minutes - subtract across midnight",
            "23:59",
            toString(subtract(create(0, 3), 4)))
test_equal("Subtract minutes - subtract more than two hours",
            "21:20",
            toString(subtract(create(0, 0), 160)))
test_equal("Subtract minutes - subtract more than two hours with borrow",
            "03:35",
            toString(subtract(create(6, 15), 160)))
test_equal("Subtract minutes - subtract more than one day (15000 mins = 25 hrs)",
            "04:32",
            toString(subtract(create(5, 32), 1500)))
test_equal("Subtract minutes - subtract more than two days",
            "00:20",
            toString(subtract(create(2, 20), 3000)))

test_true("Compare two clocks for equality - clocks with same time",
            equalClocks(create(15, 37),
                        create(15, 37)))
test_false("Compare two clocks for equality - clocks a minute apart",
            equalClocks(create(15, 36),
                        create(15, 37)))
test_false("Compare two clocks for equality - clocks an hour apart",
            equalClocks(create(14, 37),
                        create(15, 37)))
test_true("Compare two clocks for equality - clocks with hour overflow",
            equalClocks(create(10, 37),
                        create(34, 37)))
test_true("Compare two clocks for equality - clocks with hour overflow by several days",
            equalClocks(create(3, 11),
                        create(99, 11)))
test_true("Compare two clocks for equality - clocks with negative hour",
            equalClocks(create(22, 40),
                        create(-2, 40)))
test_true("Compare two clocks for equality - clocks with negative hour that wraps",
            equalClocks(create(17, 3),
                        create(-31, 3)))
test_true("Compare two clocks for equality - clocks with negative hour that wraps multiple times",
            equalClocks(create(13, 49),
                        create(-83, 49)))
test_true("Compare two clocks for equality - clocks with minute overflow",
            equalClocks(create(0, 1),
                        create(0, 1441)))
test_true("Compare two clocks for equality - clocks with minute overflow by several days",
            equalClocks(create(2, 2),
                        create(2, 4322)))
test_true("Compare two clocks for equality - clocks with negative minute",
            equalClocks(create(2, 40),
                        create(3, -20)))
test_true("Compare two clocks for equality - clocks with negative minute that wraps",
            equalClocks(create(4, 10),
                        create(5, -1490)))
test_true("Compare two clocks for equality - clocks with negative minute that wraps multiple times",
            equalClocks(create(6, 15),
                        create(6, -4305)))
test_true("Compare two clocks for equality - clocks with negative hours and minutes",
            equalClocks(create(7, 32),
                        create(-12, -268)))
test_true("Compare two clocks for equality - clocks with negative hours and minutes that wrap",
            equalClocks(create(18, 7),
                        create(-54, -11513)))
test_true("Compare two clocks for equality - full clock and zeroed clock",
            equalClocks(create(24, 0),
                        create(0, 0)))


test_report()
