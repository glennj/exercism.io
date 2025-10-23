import "./assert" for Assert
import "./timestamp" for Timestamp

class Stopwatch {
  construct new() {
    initialize_()
  }

  initialize_() {
    _laps = []
    _lapTime = 0
    _total = 0
    _state = "ready"
  }

  state() { _state }

  currentLap() { Timestamp.fromSeconds(_lapTime) }

  previousLaps() { _laps }

  total() { Timestamp.fromSeconds(_lapTime + _total) }

  start() {
    Assert.refute(_state == "running", "cannot start an already running stopwatch")
    _state = "running"
  }

  stop() {
    Assert.assert(_state == "running", "cannot stop a stopwatch that is not running")
    _state = "stopped"
  }

  reset() {
    Assert.assert(_state == "stopped", "cannot reset a stopwatch that is not stopped")
    initialize_()
  }

  advanceTime(timestamp) {
    if (_state != "running") return
    _lapTime = _lapTime + Timestamp.toSeconds(timestamp)
  }

  lap() {
    Assert.assert(_state == "running", "cannot lap a stopwatch that is not running")
    _laps.add(currentLap())
    _total = _total + _lapTime
    _lapTime = 0
  }
}
