import "./split-second-stopwatch" for Stopwatch
import "wren-testie/testie" for Testie, Expect

Testie.test("Split Second Stopwatch") { |do, skip|
  do.test("new stopwatch starts in ready state") {
    var stopwatch = Stopwatch.new()
    Expect.value(stopwatch.state()).toEqual("ready")
  }

  do.test("new stopwatch's current lap has no elapsed time") {
    var stopwatch = Stopwatch.new()
    Expect.value(stopwatch.currentLap()).toEqual("00:00:00")
  }

  do.test("new stopwatch's total has no elapsed time") {
    var stopwatch = Stopwatch.new()
    Expect.value(stopwatch.total()).toEqual("00:00:00")
  }

  do.test("new stopwatch does not have previous laps") {
    var stopwatch = Stopwatch.new()
    Expect.value(stopwatch.previousLaps()).toEqual([])
  }

  do.test("start from ready state changes state to running") {
    var stopwatch = Stopwatch.new()
    stopwatch.start()
    Expect.value(stopwatch.state()).toEqual("running")
  }

  do.test("start does not change previous laps") {
    var stopwatch = Stopwatch.new()
    stopwatch.start()
    Expect.value(stopwatch.previousLaps()).toEqual([])
  }

  do.test("start initiates time tracking for current lap") {
    var stopwatch = Stopwatch.new()
    stopwatch.start()
    stopwatch.advanceTime("00:00:05")
    Expect.value(stopwatch.currentLap()).toEqual("00:00:05")
  }

  do.test("start initiates time tracking for total") {
    var stopwatch = Stopwatch.new()
    stopwatch.start()
    stopwatch.advanceTime("00:00:23")
    Expect.value(stopwatch.total()).toEqual("00:00:23")
  }

  do.test("start cannot be called from running state") {
    var stopwatch = Stopwatch.new()
    stopwatch.start()
    Expect.that {
      stopwatch.start()
    }.abortsWith("cannot start an already running stopwatch")
  }

  do.test("stop from running state changes state to stopped") {
    var stopwatch = Stopwatch.new()
    stopwatch.start()
    stopwatch.stop()
    Expect.value(stopwatch.state()).toEqual("stopped")
  }

  do.test("stop pauses time tracking for current lap") {
    var stopwatch = Stopwatch.new()
    stopwatch.start()
    stopwatch.advanceTime("00:00:05")
    stopwatch.stop()
    stopwatch.advanceTime("00:00:08")
    Expect.value(stopwatch.currentLap()).toEqual("00:00:05")
  }

  do.test("stop pauses time tracking for total") {
    var stopwatch = Stopwatch.new()
    stopwatch.start()
    stopwatch.advanceTime("00:00:13")
    stopwatch.stop()
    stopwatch.advanceTime("00:00:44")
    Expect.value(stopwatch.total()).toEqual("00:00:13")
  }

  do.test("stop cannot be called from ready state") {
    var stopwatch = Stopwatch.new()
    Expect.that {
      stopwatch.stop()
    }.abortsWith("cannot stop a stopwatch that is not running")
  }

  do.test("stop cannot be called from stopped state") {
    var stopwatch = Stopwatch.new()
    stopwatch.start()
    stopwatch.stop()
    Expect.that {
      stopwatch.stop()
    }.abortsWith("cannot stop a stopwatch that is not running")
  }

  do.test("start from stopped state changes state to running") {
    var stopwatch = Stopwatch.new()
    stopwatch.start()
    stopwatch.stop()
    stopwatch.start()
    Expect.value(stopwatch.state()).toEqual("running")
  }

  do.test("start from stopped state resumes time tracking for current lap") {
    var stopwatch = Stopwatch.new()
    stopwatch.start()
    stopwatch.advanceTime("00:01:20")
    stopwatch.stop()
    stopwatch.advanceTime("00:00:20")
    stopwatch.start()
    stopwatch.advanceTime("00:00:08")
    Expect.value(stopwatch.currentLap()).toEqual("00:01:28")
  }

  do.test("start from stopped state resumes time tracking for total") {
    var stopwatch = Stopwatch.new()
    stopwatch.start()
    stopwatch.advanceTime("00:00:23")
    stopwatch.stop()
    stopwatch.advanceTime("00:00:44")
    stopwatch.start()
    stopwatch.advanceTime("00:00:09")
    Expect.value(stopwatch.total()).toEqual("00:00:32")
  }

  do.test("lap adds current lap to previous laps") {
    var stopwatch = Stopwatch.new()
    stopwatch.start()
    stopwatch.advanceTime("00:01:38")
    stopwatch.lap()
    Expect.value(stopwatch.previousLaps()).toEqual(["00:01:38"])
    stopwatch.advanceTime("00:00:44")
    stopwatch.lap()
    Expect.value(stopwatch.previousLaps()).toEqual(["00:01:38", "00:00:44"])
  }

  do.test("lap resets current lap and resumes time tracking") {
    var stopwatch = Stopwatch.new()
    stopwatch.start()
    stopwatch.advanceTime("00:08:22")
    stopwatch.lap()
    Expect.value(stopwatch.currentLap()).toEqual("00:00:00")
    stopwatch.advanceTime("00:00:15")
    Expect.value(stopwatch.currentLap()).toEqual("00:00:15")
  }

  do.test("lap continues time tracking for total") {
    var stopwatch = Stopwatch.new()
    stopwatch.start()
    stopwatch.advanceTime("00:00:22")
    stopwatch.lap()
    stopwatch.advanceTime("00:00:33")
    Expect.value(stopwatch.total()).toEqual("00:00:55")
  }

  do.test("lap cannot be called from ready state") {
    var stopwatch = Stopwatch.new()
    Expect.that {
      stopwatch.lap()
    }.abortsWith("cannot lap a stopwatch that is not running")
  }

  do.test("lap cannot be called from stopped state") {
    var stopwatch = Stopwatch.new()
    stopwatch.start()
    stopwatch.stop()
    Expect.that {
      stopwatch.lap()
    }.abortsWith("cannot lap a stopwatch that is not running")
  }

  do.test("stop does not change previous laps") {
    var stopwatch = Stopwatch.new()
    stopwatch.start()
    stopwatch.advanceTime("00:11:22")
    stopwatch.lap()
    Expect.value(stopwatch.previousLaps()).toEqual(["00:11:22"])
    stopwatch.stop()
    Expect.value(stopwatch.previousLaps()).toEqual(["00:11:22"])
  }

  do.test("reset from stopped state changes state to ready") {
    var stopwatch = Stopwatch.new()
    stopwatch.start()
    stopwatch.stop()
    stopwatch.reset()
    Expect.value(stopwatch.state()).toEqual("ready")
  }

  do.test("reset resets current lap") {
    var stopwatch = Stopwatch.new()
    stopwatch.start()
    stopwatch.advanceTime("00:00:10")
    stopwatch.stop()
    stopwatch.reset()
    Expect.value(stopwatch.currentLap()).toEqual("00:00:00")
  }

  do.test("reset clears previous laps") {
    var stopwatch = Stopwatch.new()
    stopwatch.start()
    stopwatch.advanceTime("00:00:10")
    stopwatch.lap()
    stopwatch.advanceTime("00:00:20")
    stopwatch.lap()
    Expect.value(stopwatch.previousLaps()).toEqual(["00:00:10", "00:00:20"])
    stopwatch.stop()
    stopwatch.reset()
    Expect.value(stopwatch.previousLaps()).toEqual([])
  }

  do.test("reset cannot be called from ready state") {
    var stopwatch = Stopwatch.new()
    Expect.that {
      stopwatch.reset()
    }.abortsWith("cannot reset a stopwatch that is not stopped")
  }

  do.test("reset cannot be called from running state") {
    var stopwatch = Stopwatch.new()
    stopwatch.start()
    Expect.that {
      stopwatch.reset()
    }.abortsWith("cannot reset a stopwatch that is not stopped")
  }

  do.test("supports very long laps") {
    var stopwatch = Stopwatch.new()
    stopwatch.start()
    stopwatch.advanceTime("01:23:45")
    Expect.value(stopwatch.currentLap()).toEqual("01:23:45")
    stopwatch.lap()
    Expect.value(stopwatch.previousLaps()).toEqual(["01:23:45"])
    stopwatch.advanceTime("04:01:40")
    Expect.value(stopwatch.currentLap()).toEqual("04:01:40")
    Expect.value(stopwatch.total()).toEqual("05:25:25")
    stopwatch.lap()
    Expect.value(stopwatch.previousLaps()).toEqual(["01:23:45", "04:01:40"])
    stopwatch.advanceTime("08:43:05")
    Expect.value(stopwatch.currentLap()).toEqual("08:43:05")
    Expect.value(stopwatch.total()).toEqual("14:08:30")
    stopwatch.lap()
    Expect.value(stopwatch.previousLaps()).toEqual(["01:23:45", "04:01:40", "08:43:05"])
  }
}
