#!/usr/bin/env bash

@test 'date only specificaion of time' {
  run bash gigasecond.sh '2011-04-25Z'

  [ "$status" -eq 0 ]
  [ "$output" == 'Thu Jan 1 01:46:40 UTC 2043' ]
}

@test "no args" {
    run bash gigasecond.sh
    (( status != 0 ))
    [[ $output == "usage"* ]]
}

@test "invalid datetime" {
    run bash gigasecond.sh "foo bar"
    (( status != 0 ))
    [[ $output == *"invalid time-spec"* ]]
}

@test 'second test for date only specification of time' {
  run bash gigasecond.sh '1977-06-13Z'

  [ "$status" -eq 0 ]
  [ "$output" == 'Thu Feb 19 01:46:40 UTC 2009' ]
}

@test 'third test for date only specification of time' {
  run bash gigasecond.sh '1959-07-19Z'

  [ "$status" -eq 0 ]
  [ "$output" == 'Wed Mar 27 01:46:40 UTC 1991' ]
}

@test 'full time specified' {
  run bash gigasecond.sh '2015-01-24 22:00:00Z'

  [ "$status" -eq 0 ]
  [ "$output" == 'Tue Oct 2 23:46:40 UTC 2046' ]
}

@test 'full time with day roll-over' {
  run bash gigasecond.sh '2015-01-24 23:59:59Z'

  [ "$status" -eq 0 ]
  [ "$output" == 'Wed Oct 3 01:46:39 UTC 2046' ]
}
