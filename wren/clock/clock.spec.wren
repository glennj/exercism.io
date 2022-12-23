import "wren-testie/testie" for Testie, Expect
import "./clock" for Clock

Testie.test("Clock") { |do, skip|

  do.describe("Create a new clock with an initial time") {
    do.test("on the hour") {
      var c = Clock.new(8, 0)
      Expect.value(c.toString).toEqual("08:00")
    }
    skip.test("past the hour") {
      var c = Clock.new(11, 9)
      Expect.value(c.toString).toEqual("11:09")
    }
    skip.test("midnight is zero hours") {
      var c = Clock.new(24, 0)
      Expect.value(c.toString).toEqual("00:00")
    }
    skip.test("hour rolls over") {
      var c = Clock.new(25, 0)
      Expect.value(c.toString).toEqual("01:00")
    }
    skip.test("hour rolls over continuously") {
      var c = Clock.new(100, 0)
      Expect.value(c.toString).toEqual("04:00")
    }
    skip.test("sixty minutes is next hour") {
      var c = Clock.new(1, 60)
      Expect.value(c.toString).toEqual("02:00")
    }
    skip.test("minutes roll over") {
      var c = Clock.new(0, 160)
      Expect.value(c.toString).toEqual("02:40")
    }
    skip.test("minutes roll over continuously") {
      var c = Clock.new(0, 1723)
      Expect.value(c.toString).toEqual("04:43")
    }
    skip.test("hour and minutes roll over") {
      var c = Clock.new(25, 160)
      Expect.value(c.toString).toEqual("03:40")
    }
    skip.test("hour and minutes roll over continuously") {
      var c = Clock.new(201, 3001)
      Expect.value(c.toString).toEqual("11:01")
    }
    skip.test("hour and minutes roll over to exactly midnight") {
      var c = Clock.new(72, 8640)
      Expect.value(c.toString).toEqual("00:00")
    }
    skip.test("negative hour") {
      var c = Clock.new(-1, 15)
      Expect.value(c.toString).toEqual("23:15")
    }
    skip.test("negative hour rolls over") {
      var c = Clock.new(-25, 0)
      Expect.value(c.toString).toEqual("23:00")
    }
    skip.test("negative hour rolls over continuously") {
      var c = Clock.new(-91, 0)
      Expect.value(c.toString).toEqual("05:00")
    }
    skip.test("negative minutes") {
      var c = Clock.new(1, -40)
      Expect.value(c.toString).toEqual("00:20")
    }
    skip.test("negative minutes roll over") {
      var c = Clock.new(1, -160)
      Expect.value(c.toString).toEqual("22:20")
    }
    skip.test("negative minutes roll over continuously") {
      var c = Clock.new(1, -4820)
      Expect.value(c.toString).toEqual("16:40")
    }
    skip.test("negative sixty minutes is previous hour") {
      var c = Clock.new(2, -60)
      Expect.value(c.toString).toEqual("01:00")
    }
    skip.test("negative hour and minutes both roll over") {
      var c = Clock.new(-25, -160)
      Expect.value(c.toString).toEqual("20:20")
    }
    skip.test("negative hour and minutes both roll over continuously") {
      var c = Clock.new(-121, -5810)
      Expect.value(c.toString).toEqual("22:10")
    }  
  }

  do.describe("Add minutes") {
    skip.test("add minutes") {
      var c = Clock.new(10, 0)
      c.add(3)
      Expect.value(c.toString).toEqual("10:03")
    }
    skip.test("add no minutes") {
      var c = Clock.new(6, 41)
      c.add(0)
      Expect.value(c.toString).toEqual("06:41")
    }
    skip.test("add to next hour") {
      var c = Clock.new(0, 45)
      c.add(40)
      Expect.value(c.toString).toEqual("01:25")
    }
    skip.test("add more than one hour") {
      var c = Clock.new(10, 0)
      c.add(61)
      Expect.value(c.toString).toEqual("11:01")
    }
    skip.test("add more than two hours with carry") {
      var c = Clock.new(0, 45)
      c.add(160)
      Expect.value(c.toString).toEqual("03:25")
    }
    skip.test("add across midnight") {
      var c = Clock.new(23, 59)
      c.add(2)
      Expect.value(c.toString).toEqual("00:01")
    }
    skip.test("add more than one day (1500 min = 25 hrs)") {
      var c = Clock.new(5, 32)
      c.add(1500)
      Expect.value(c.toString).toEqual("06:32")
    }
    skip.test("add more than two days") {
      var c = Clock.new(1, 1)
      c.add(3500)
      Expect.value(c.toString).toEqual("11:21")
    }
  }

  do.describe("Subtract minutes") {
    skip.test("subtract minutes") {
      var c = Clock.new(10, 3)
      c.subtract(3)
      Expect.value(c.toString).toEqual("10:00")
    }
    skip.test("subtract to previous hour") {
      var c = Clock.new(10, 3)
      c.subtract(30)
      Expect.value(c.toString).toEqual("09:33")
    }
    skip.test("subtract more than an hour") {
      var c = Clock.new(10, 3)
      c.subtract(70)
      Expect.value(c.toString).toEqual("08:53")
    }
    skip.test("subtract across midnight") {
      var c = Clock.new(0, 3)
      c.subtract(4)
      Expect.value(c.toString).toEqual("23:59")
    }
    skip.test("subtract more than two hours") {
      var c = Clock.new(0, 0)
      c.subtract(160)
      Expect.value(c.toString).toEqual("21:20")
    }
    skip.test("subtract more than two hours with borrow") {
      var c = Clock.new(6, 15)
      c.subtract(160)
      Expect.value(c.toString).toEqual("03:35")
    }
    skip.test("subtract more than one day (1500 min = 25 hrs)") {
      var c = Clock.new(5, 32)
      c.subtract(1500)
      Expect.value(c.toString).toEqual("04:32")
    }
    skip.test("subtract more than two days") {
      var c = Clock.new(2, 20)
      c.subtract(3000)
      Expect.value(c.toString).toEqual("00:20")
    }
  }

  do.describe("Compare two clocks for equality") {
    skip.test("clocks with same time") {
      var clock1 = Clock.new(15, 37)
      var clock2 = Clock.new(15, 37)
      Expect.value(clock1 == clock2).toBe(true)
    }
    skip.test("clocks a minute apart") {
      var clock1 = Clock.new(15, 36)
      var clock2 = Clock.new(15, 37)
      Expect.value(clock1 == clock2).toBe(false)
    }
    skip.test("clocks an hour apart") {
      var clock1 = Clock.new(14, 37)
      var clock2 = Clock.new(15, 37)
      Expect.value(clock1 == clock2).toBe(false)
    }
    skip.test("clocks with hour overflow") {
      var clock1 = Clock.new(10, 37)
      var clock2 = Clock.new(34, 37)
      Expect.value(clock1 == clock2).toBe(true)
    }
    skip.test("clocks with hour overflow by several days") {
      var clock1 = Clock.new(3, 11)
      var clock2 = Clock.new(99, 11)
      Expect.value(clock1 == clock2).toBe(true)
    }
    skip.test("clocks with negative hour") {
      var clock1 = Clock.new(22, 40)
      var clock2 = Clock.new(-2, 40)
      Expect.value(clock1 == clock2).toBe(true)
    }
    skip.test("clocks with negative hour that wraps") {
      var clock1 = Clock.new(17, 3)
      var clock2 = Clock.new(-31, 3)
      Expect.value(clock1 == clock2).toBe(true)
    }
    skip.test("clocks with negative hour that wraps multiple times") {
      var clock1 = Clock.new(13, 49)
      var clock2 = Clock.new(-83, 49)
      Expect.value(clock1 == clock2).toBe(true)
    }
    skip.test("clocks with minute overflow") {
      var clock1 = Clock.new(0, 1)
      var clock2 = Clock.new(0, 1441)
      Expect.value(clock1 == clock2).toBe(true)
    }
    skip.test("clocks with minute overflow by several days") {
      var clock1 = Clock.new(2, 2)
      var clock2 = Clock.new(2, 4322)
      Expect.value(clock1 == clock2).toBe(true)
    }
    skip.test("clocks with negative minute") {
      var clock1 = Clock.new(2, 40)
      var clock2 = Clock.new(3, -20)
      Expect.value(clock1 == clock2).toBe(true)
    }
    skip.test("clocks with negative minute that wraps") {
      var clock1 = Clock.new(4, 10)
      var clock2 = Clock.new(5, -1490)
      Expect.value(clock1 == clock2).toBe(true)
    }
    skip.test("clocks with negative minute that wraps multiple times") {
      var clock1 = Clock.new(6, 15)
      var clock2 = Clock.new(6, -4305)
      Expect.value(clock1 == clock2).toBe(true)
    }
    skip.test("clocks with negative hours and minutes") {
      var clock1 = Clock.new(7, 32)
      var clock2 = Clock.new(-12, -268)
      Expect.value(clock1 == clock2).toBe(true)
    }
    skip.test("clocks with negative hours and minutes that wrap") {
      var clock1 = Clock.new(18, 7)
      var clock2 = Clock.new(-54, -11513)
      Expect.value(clock1 == clock2).toBe(true)
    }
    skip.test("full clock and zeroed clock") {
      var clock1 = Clock.new(24, 0)
      var clock2 = Clock.new(0, 0)
      Expect.value(clock1 == clock2).toBe(true)
    }
  }
}
