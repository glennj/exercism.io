SplitSecondStopwatch = require 'split_second_stopwatch'

describe 'split-second-stopwatch', ->
  it 'new stopwatch starts in ready state', ->
    stopwatch = SplitSecondStopwatch.Stopwatch!
    assert.same 'ready', stopwatch\state!

  it "new stopwatch's current lap has no elapsed time", ->
    stopwatch = SplitSecondStopwatch.Stopwatch!
    assert.same '00:00:00', stopwatch\currentLap!

  it "new stopwatch's total has no elapsed time", ->
    stopwatch = SplitSecondStopwatch.Stopwatch!
    assert.same '00:00:00', stopwatch\total!

  it 'new stopwatch does not have previous laps', ->
    stopwatch = SplitSecondStopwatch.Stopwatch!
    assert.same {}, stopwatch\previousLaps!

  it 'start from ready state changes state to running', ->
    stopwatch = SplitSecondStopwatch.Stopwatch!
    stopwatch\start!
    assert.same 'running', stopwatch\state!

  it 'start does not change previous laps', ->
    stopwatch = SplitSecondStopwatch.Stopwatch!
    stopwatch\start!
    assert.same {}, stopwatch\previousLaps!

  it 'start initiates time tracking for current lap', ->
    stopwatch = SplitSecondStopwatch.Stopwatch!
    stopwatch\start!
    stopwatch\advanceTime '00:00:05'
    assert.same '00:00:05', stopwatch\currentLap!

  it 'start initiates time tracking for total', ->
    stopwatch = SplitSecondStopwatch.Stopwatch!
    stopwatch\start!
    stopwatch\advanceTime '00:00:23'
    assert.same '00:00:23', stopwatch\total!

  it 'start cannot be called from running state', ->
    stopwatch = SplitSecondStopwatch.Stopwatch!
    stopwatch\start!
    assert.has_error (-> stopwatch\start!), 'cannot start an already running stopwatch'

  it 'stop from running state changes state to stopped', ->
    stopwatch = SplitSecondStopwatch.Stopwatch!
    stopwatch\start!
    stopwatch\stop!
    assert.same 'stopped', stopwatch\state!

  it 'stop pauses time tracking for current lap', ->
    stopwatch = SplitSecondStopwatch.Stopwatch!
    stopwatch\start!
    stopwatch\advanceTime '00:00:05'
    stopwatch\stop!
    stopwatch\advanceTime '00:00:08'
    assert.same '00:00:05', stopwatch\currentLap!

  it 'stop pauses time tracking for total', ->
    stopwatch = SplitSecondStopwatch.Stopwatch!
    stopwatch\start!
    stopwatch\advanceTime '00:00:13'
    stopwatch\stop!
    stopwatch\advanceTime '00:00:44'
    assert.same '00:00:13', stopwatch\total!

  it 'stop cannot be called from ready state', ->
    stopwatch = SplitSecondStopwatch.Stopwatch!
    assert.has_error (-> stopwatch\stop!), 'cannot stop a stopwatch that is not running'

  it 'stop cannot be called from stopped state', ->
    stopwatch = SplitSecondStopwatch.Stopwatch!
    stopwatch\start!
    stopwatch\stop!
    assert.has_error (-> stopwatch\stop!), 'cannot stop a stopwatch that is not running'

  it 'start from stopped state changes state to running', ->
    stopwatch = SplitSecondStopwatch.Stopwatch!
    stopwatch\start!
    stopwatch\stop!
    stopwatch\start!
    assert.same 'running', stopwatch\state!

  it 'start from stopped state resumes time tracking for current lap', ->
    stopwatch = SplitSecondStopwatch.Stopwatch!
    stopwatch\start!
    stopwatch\advanceTime '00:01:20'
    stopwatch\stop!
    stopwatch\advanceTime '00:00:20'
    stopwatch\start!
    stopwatch\advanceTime '00:00:08'
    assert.same '00:01:28', stopwatch\currentLap!

  it 'start from stopped state resumes time tracking for total', ->
    stopwatch = SplitSecondStopwatch.Stopwatch!
    stopwatch\start!
    stopwatch\advanceTime '00:00:23'
    stopwatch\stop!
    stopwatch\advanceTime '00:00:44'
    stopwatch\start!
    stopwatch\advanceTime '00:00:09'
    assert.same '00:00:32', stopwatch\total!

  it 'lap adds current lap to previous laps', ->
    stopwatch = SplitSecondStopwatch.Stopwatch!
    stopwatch\start!
    stopwatch\advanceTime '00:01:38'
    stopwatch\lap!
    assert.same { '00:01:38' }, stopwatch\previousLaps!
    stopwatch\advanceTime '00:00:44'
    stopwatch\lap!
    assert.same { '00:01:38', '00:00:44' }, stopwatch\previousLaps!

  it 'lap resets current lap and resumes time tracking', ->
    stopwatch = SplitSecondStopwatch.Stopwatch!
    stopwatch\start!
    stopwatch\advanceTime '00:08:22'
    stopwatch\lap!
    assert.same '00:00:00', stopwatch\currentLap!
    stopwatch\advanceTime '00:00:15'
    assert.same '00:00:15', stopwatch\currentLap!

  it 'lap continues time tracking for total', ->
    stopwatch = SplitSecondStopwatch.Stopwatch!
    stopwatch\start!
    stopwatch\advanceTime '00:00:22'
    stopwatch\lap!
    stopwatch\advanceTime '00:00:33'
    assert.same '00:00:55', stopwatch\total!

  it 'lap cannot be called from ready state', ->
    stopwatch = SplitSecondStopwatch.Stopwatch!
    assert.has_error (-> stopwatch\lap!), 'cannot lap a stopwatch that is not running'

  it 'lap cannot be called from stopped state', ->
    stopwatch = SplitSecondStopwatch.Stopwatch!
    stopwatch\start!
    stopwatch\stop!
    assert.has_error (-> stopwatch\lap!), 'cannot lap a stopwatch that is not running'

  it 'stop does not change previous laps', ->
    stopwatch = SplitSecondStopwatch.Stopwatch!
    stopwatch\start!
    stopwatch\advanceTime '00:11:22'
    stopwatch\lap!
    assert.same { '00:11:22' }, stopwatch\previousLaps!
    stopwatch\stop!
    assert.same { '00:11:22' }, stopwatch\previousLaps!

  it 'reset from stopped state changes state to ready', ->
    stopwatch = SplitSecondStopwatch.Stopwatch!
    stopwatch\start!
    stopwatch\stop!
    stopwatch\reset!
    assert.same 'ready', stopwatch\state!

  it 'reset resets current lap', ->
    stopwatch = SplitSecondStopwatch.Stopwatch!
    stopwatch\start!
    stopwatch\advanceTime '00:00:10'
    stopwatch\stop!
    stopwatch\reset!
    assert.same '00:00:00', stopwatch\currentLap!

  it 'reset clears previous laps', ->
    stopwatch = SplitSecondStopwatch.Stopwatch!
    stopwatch\start!
    stopwatch\advanceTime '00:00:10'
    stopwatch\lap!
    stopwatch\advanceTime '00:00:20'
    stopwatch\lap!
    assert.same { '00:00:10', '00:00:20' }, stopwatch\previousLaps!
    stopwatch\stop!
    stopwatch\reset!
    assert.same {}, stopwatch\previousLaps!

  it 'reset cannot be called from ready state', ->
    stopwatch = SplitSecondStopwatch.Stopwatch!
    assert.has_error (-> stopwatch\reset!), 'cannot reset a stopwatch that is not stopped'

  it 'reset cannot be called from running state', ->
    stopwatch = SplitSecondStopwatch.Stopwatch!
    stopwatch\start!
    assert.has_error (-> stopwatch\reset!), 'cannot reset a stopwatch that is not stopped'

  it 'supports very long laps', ->
    stopwatch = SplitSecondStopwatch.Stopwatch!
    stopwatch\start!
    stopwatch\advanceTime '01:23:45'
    assert.same '01:23:45', stopwatch\currentLap!
    stopwatch\lap!
    assert.same { '01:23:45' }, stopwatch\previousLaps!
    stopwatch\advanceTime '04:01:40'
    assert.same '04:01:40', stopwatch\currentLap!
    assert.same '05:25:25', stopwatch\total!
    stopwatch\lap!
    assert.same { '01:23:45', '04:01:40' }, stopwatch\previousLaps!
    stopwatch\advanceTime '08:43:05'
    assert.same '08:43:05', stopwatch\currentLap!
    assert.same '14:08:30', stopwatch\total!
    stopwatch\lap!
    assert.same { '01:23:45', '04:01:40', '08:43:05' }, stopwatch\previousLaps!
