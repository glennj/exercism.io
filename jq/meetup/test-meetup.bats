#!/usr/bin/env bats
# generated on 2022-11-02T20:59:18Z
load bats-extra

@test 'when teenth Monday is the 13th, the first day of the teenth week' {
    #[[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f meetup.jq << 'END_INPUT'
        {
          "year": 2013,
          "month": 5,
          "week": "teenth",
          "dayofweek": "Monday"
        }
END_INPUT

    assert_success
    expected='2013-05-13'
    assert_equal "$output" "$expected"
}

@test 'when teenth Monday is the 19th, the last day of the teenth week' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f meetup.jq << 'END_INPUT'
        {
          "year": 2013,
          "month": 8,
          "week": "teenth",
          "dayofweek": "Monday"
        }
END_INPUT

    assert_success
    expected='2013-08-19'
    assert_equal "$output" "$expected"
}

@test 'when teenth Monday is some day in the middle of the teenth week' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f meetup.jq << 'END_INPUT'
        {
          "year": 2013,
          "month": 9,
          "week": "teenth",
          "dayofweek": "Monday"
        }
END_INPUT

    assert_success
    expected='2013-09-16'
    assert_equal "$output" "$expected"
}

@test 'when teenth Tuesday is the 19th, the last day of the teenth week' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f meetup.jq << 'END_INPUT'
        {
          "year": 2013,
          "month": 3,
          "week": "teenth",
          "dayofweek": "Tuesday"
        }
END_INPUT

    assert_success
    expected='2013-03-19'
    assert_equal "$output" "$expected"
}

@test 'when teenth Tuesday is some day in the middle of the teenth week' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f meetup.jq << 'END_INPUT'
        {
          "year": 2013,
          "month": 4,
          "week": "teenth",
          "dayofweek": "Tuesday"
        }
END_INPUT

    assert_success
    expected='2013-04-16'
    assert_equal "$output" "$expected"
}

@test 'when teenth Tuesday is the 13th, the first day of the teenth week' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f meetup.jq << 'END_INPUT'
        {
          "year": 2013,
          "month": 8,
          "week": "teenth",
          "dayofweek": "Tuesday"
        }
END_INPUT

    assert_success
    expected='2013-08-13'
    assert_equal "$output" "$expected"
}

@test 'when teenth Wednesday is some day in the middle of the teenth week' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f meetup.jq << 'END_INPUT'
        {
          "year": 2013,
          "month": 1,
          "week": "teenth",
          "dayofweek": "Wednesday"
        }
END_INPUT

    assert_success
    expected='2013-01-16'
    assert_equal "$output" "$expected"
}

@test 'when teenth Wednesday is the 13th, the first day of the teenth week' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f meetup.jq << 'END_INPUT'
        {
          "year": 2013,
          "month": 2,
          "week": "teenth",
          "dayofweek": "Wednesday"
        }
END_INPUT

    assert_success
    expected='2013-02-13'
    assert_equal "$output" "$expected"
}

@test 'when teenth Wednesday is the 19th, the last day of the teenth week' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f meetup.jq << 'END_INPUT'
        {
          "year": 2013,
          "month": 6,
          "week": "teenth",
          "dayofweek": "Wednesday"
        }
END_INPUT

    assert_success
    expected='2013-06-19'
    assert_equal "$output" "$expected"
}

@test 'when teenth Thursday is some day in the middle of the teenth week' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f meetup.jq << 'END_INPUT'
        {
          "year": 2013,
          "month": 5,
          "week": "teenth",
          "dayofweek": "Thursday"
        }
END_INPUT

    assert_success
    expected='2013-05-16'
    assert_equal "$output" "$expected"
}

@test 'when teenth Thursday is the 13th, the first day of the teenth week' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f meetup.jq << 'END_INPUT'
        {
          "year": 2013,
          "month": 6,
          "week": "teenth",
          "dayofweek": "Thursday"
        }
END_INPUT

    assert_success
    expected='2013-06-13'
    assert_equal "$output" "$expected"
}

@test 'when teenth Thursday is the 19th, the last day of the teenth week' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f meetup.jq << 'END_INPUT'
        {
          "year": 2013,
          "month": 9,
          "week": "teenth",
          "dayofweek": "Thursday"
        }
END_INPUT

    assert_success
    expected='2013-09-19'
    assert_equal "$output" "$expected"
}

@test 'when teenth Friday is the 19th, the last day of the teenth week' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f meetup.jq << 'END_INPUT'
        {
          "year": 2013,
          "month": 4,
          "week": "teenth",
          "dayofweek": "Friday"
        }
END_INPUT

    assert_success
    expected='2013-04-19'
    assert_equal "$output" "$expected"
}

@test 'when teenth Friday is some day in the middle of the teenth week' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f meetup.jq << 'END_INPUT'
        {
          "year": 2013,
          "month": 8,
          "week": "teenth",
          "dayofweek": "Friday"
        }
END_INPUT

    assert_success
    expected='2013-08-16'
    assert_equal "$output" "$expected"
}

@test 'when teenth Friday is the 13th, the first day of the teenth week' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f meetup.jq << 'END_INPUT'
        {
          "year": 2013,
          "month": 9,
          "week": "teenth",
          "dayofweek": "Friday"
        }
END_INPUT

    assert_success
    expected='2013-09-13'
    assert_equal "$output" "$expected"
}

@test 'when teenth Saturday is some day in the middle of the teenth week' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f meetup.jq << 'END_INPUT'
        {
          "year": 2013,
          "month": 2,
          "week": "teenth",
          "dayofweek": "Saturday"
        }
END_INPUT

    assert_success
    expected='2013-02-16'
    assert_equal "$output" "$expected"
}

@test 'when teenth Saturday is the 13th, the first day of the teenth week' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f meetup.jq << 'END_INPUT'
        {
          "year": 2013,
          "month": 4,
          "week": "teenth",
          "dayofweek": "Saturday"
        }
END_INPUT

    assert_success
    expected='2013-04-13'
    assert_equal "$output" "$expected"
}

@test 'when teenth Saturday is the 19th, the last day of the teenth week' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f meetup.jq << 'END_INPUT'
        {
          "year": 2013,
          "month": 10,
          "week": "teenth",
          "dayofweek": "Saturday"
        }
END_INPUT

    assert_success
    expected='2013-10-19'
    assert_equal "$output" "$expected"
}

@test 'when teenth Sunday is the 19th, the last day of the teenth week' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f meetup.jq << 'END_INPUT'
        {
          "year": 2013,
          "month": 5,
          "week": "teenth",
          "dayofweek": "Sunday"
        }
END_INPUT

    assert_success
    expected='2013-05-19'
    assert_equal "$output" "$expected"
}

@test 'when teenth Sunday is some day in the middle of the teenth week' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f meetup.jq << 'END_INPUT'
        {
          "year": 2013,
          "month": 6,
          "week": "teenth",
          "dayofweek": "Sunday"
        }
END_INPUT

    assert_success
    expected='2013-06-16'
    assert_equal "$output" "$expected"
}

@test 'when teenth Sunday is the 13th, the first day of the teenth week' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f meetup.jq << 'END_INPUT'
        {
          "year": 2013,
          "month": 10,
          "week": "teenth",
          "dayofweek": "Sunday"
        }
END_INPUT

    assert_success
    expected='2013-10-13'
    assert_equal "$output" "$expected"
}

@test 'when first Monday is some day in the middle of the first week' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f meetup.jq << 'END_INPUT'
        {
          "year": 2013,
          "month": 3,
          "week": "first",
          "dayofweek": "Monday"
        }
END_INPUT

    assert_success
    expected='2013-03-04'
    assert_equal "$output" "$expected"
}

@test 'when first Monday is the 1st, the first day of the first week' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f meetup.jq << 'END_INPUT'
        {
          "year": 2013,
          "month": 4,
          "week": "first",
          "dayofweek": "Monday"
        }
END_INPUT

    assert_success
    expected='2013-04-01'
    assert_equal "$output" "$expected"
}

@test 'when first Tuesday is the 7th, the last day of the first week' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f meetup.jq << 'END_INPUT'
        {
          "year": 2013,
          "month": 5,
          "week": "first",
          "dayofweek": "Tuesday"
        }
END_INPUT

    assert_success
    expected='2013-05-07'
    assert_equal "$output" "$expected"
}

@test 'when first Tuesday is some day in the middle of the first week' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f meetup.jq << 'END_INPUT'
        {
          "year": 2013,
          "month": 6,
          "week": "first",
          "dayofweek": "Tuesday"
        }
END_INPUT

    assert_success
    expected='2013-06-04'
    assert_equal "$output" "$expected"
}

@test 'when first Wednesday is some day in the middle of the first week' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f meetup.jq << 'END_INPUT'
        {
          "year": 2013,
          "month": 7,
          "week": "first",
          "dayofweek": "Wednesday"
        }
END_INPUT

    assert_success
    expected='2013-07-03'
    assert_equal "$output" "$expected"
}

@test 'when first Wednesday is the 7th, the last day of the first week' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f meetup.jq << 'END_INPUT'
        {
          "year": 2013,
          "month": 8,
          "week": "first",
          "dayofweek": "Wednesday"
        }
END_INPUT

    assert_success
    expected='2013-08-07'
    assert_equal "$output" "$expected"
}

@test 'when first Thursday is some day in the middle of the first week' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f meetup.jq << 'END_INPUT'
        {
          "year": 2013,
          "month": 9,
          "week": "first",
          "dayofweek": "Thursday"
        }
END_INPUT

    assert_success
    expected='2013-09-05'
    assert_equal "$output" "$expected"
}

@test 'when first Thursday is another day in the middle of the first week' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f meetup.jq << 'END_INPUT'
        {
          "year": 2013,
          "month": 10,
          "week": "first",
          "dayofweek": "Thursday"
        }
END_INPUT

    assert_success
    expected='2013-10-03'
    assert_equal "$output" "$expected"
}

@test 'when first Friday is the 1st, the first day of the first week' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f meetup.jq << 'END_INPUT'
        {
          "year": 2013,
          "month": 11,
          "week": "first",
          "dayofweek": "Friday"
        }
END_INPUT

    assert_success
    expected='2013-11-01'
    assert_equal "$output" "$expected"
}

@test 'when first Friday is some day in the middle of the first week' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f meetup.jq << 'END_INPUT'
        {
          "year": 2013,
          "month": 12,
          "week": "first",
          "dayofweek": "Friday"
        }
END_INPUT

    assert_success
    expected='2013-12-06'
    assert_equal "$output" "$expected"
}

@test 'when first Saturday is some day in the middle of the first week' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f meetup.jq << 'END_INPUT'
        {
          "year": 2013,
          "month": 1,
          "week": "first",
          "dayofweek": "Saturday"
        }
END_INPUT

    assert_success
    expected='2013-01-05'
    assert_equal "$output" "$expected"
}

@test 'when first Saturday is another day in the middle of the first week' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f meetup.jq << 'END_INPUT'
        {
          "year": 2013,
          "month": 2,
          "week": "first",
          "dayofweek": "Saturday"
        }
END_INPUT

    assert_success
    expected='2013-02-02'
    assert_equal "$output" "$expected"
}

@test 'when first Sunday is some day in the middle of the first week' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f meetup.jq << 'END_INPUT'
        {
          "year": 2013,
          "month": 3,
          "week": "first",
          "dayofweek": "Sunday"
        }
END_INPUT

    assert_success
    expected='2013-03-03'
    assert_equal "$output" "$expected"
}

@test 'when first Sunday is the 7th, the last day of the first week' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f meetup.jq << 'END_INPUT'
        {
          "year": 2013,
          "month": 4,
          "week": "first",
          "dayofweek": "Sunday"
        }
END_INPUT

    assert_success
    expected='2013-04-07'
    assert_equal "$output" "$expected"
}

@test 'when second Monday is some day in the middle of the second week' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f meetup.jq << 'END_INPUT'
        {
          "year": 2013,
          "month": 3,
          "week": "second",
          "dayofweek": "Monday"
        }
END_INPUT

    assert_success
    expected='2013-03-11'
    assert_equal "$output" "$expected"
}

@test 'when second Monday is the 8th, the first day of the second week' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f meetup.jq << 'END_INPUT'
        {
          "year": 2013,
          "month": 4,
          "week": "second",
          "dayofweek": "Monday"
        }
END_INPUT

    assert_success
    expected='2013-04-08'
    assert_equal "$output" "$expected"
}

@test 'when second Tuesday is the 14th, the last day of the second week' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f meetup.jq << 'END_INPUT'
        {
          "year": 2013,
          "month": 5,
          "week": "second",
          "dayofweek": "Tuesday"
        }
END_INPUT

    assert_success
    expected='2013-05-14'
    assert_equal "$output" "$expected"
}

@test 'when second Tuesday is some day in the middle of the second week' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f meetup.jq << 'END_INPUT'
        {
          "year": 2013,
          "month": 6,
          "week": "second",
          "dayofweek": "Tuesday"
        }
END_INPUT

    assert_success
    expected='2013-06-11'
    assert_equal "$output" "$expected"
}

@test 'when second Wednesday is some day in the middle of the second week' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f meetup.jq << 'END_INPUT'
        {
          "year": 2013,
          "month": 7,
          "week": "second",
          "dayofweek": "Wednesday"
        }
END_INPUT

    assert_success
    expected='2013-07-10'
    assert_equal "$output" "$expected"
}

@test 'when second Wednesday is the 14th, the last day of the second week' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f meetup.jq << 'END_INPUT'
        {
          "year": 2013,
          "month": 8,
          "week": "second",
          "dayofweek": "Wednesday"
        }
END_INPUT

    assert_success
    expected='2013-08-14'
    assert_equal "$output" "$expected"
}

@test 'when second Thursday is some day in the middle of the second week' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f meetup.jq << 'END_INPUT'
        {
          "year": 2013,
          "month": 9,
          "week": "second",
          "dayofweek": "Thursday"
        }
END_INPUT

    assert_success
    expected='2013-09-12'
    assert_equal "$output" "$expected"
}

@test 'when second Thursday is another day in the middle of the second week' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f meetup.jq << 'END_INPUT'
        {
          "year": 2013,
          "month": 10,
          "week": "second",
          "dayofweek": "Thursday"
        }
END_INPUT

    assert_success
    expected='2013-10-10'
    assert_equal "$output" "$expected"
}

@test 'when second Friday is the 8th, the first day of the second week' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f meetup.jq << 'END_INPUT'
        {
          "year": 2013,
          "month": 11,
          "week": "second",
          "dayofweek": "Friday"
        }
END_INPUT

    assert_success
    expected='2013-11-08'
    assert_equal "$output" "$expected"
}

@test 'when second Friday is some day in the middle of the second week' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f meetup.jq << 'END_INPUT'
        {
          "year": 2013,
          "month": 12,
          "week": "second",
          "dayofweek": "Friday"
        }
END_INPUT

    assert_success
    expected='2013-12-13'
    assert_equal "$output" "$expected"
}

@test 'when second Saturday is some day in the middle of the second week' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f meetup.jq << 'END_INPUT'
        {
          "year": 2013,
          "month": 1,
          "week": "second",
          "dayofweek": "Saturday"
        }
END_INPUT

    assert_success
    expected='2013-01-12'
    assert_equal "$output" "$expected"
}

@test 'when second Saturday is another day in the middle of the second week' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f meetup.jq << 'END_INPUT'
        {
          "year": 2013,
          "month": 2,
          "week": "second",
          "dayofweek": "Saturday"
        }
END_INPUT

    assert_success
    expected='2013-02-09'
    assert_equal "$output" "$expected"
}

@test 'when second Sunday is some day in the middle of the second week' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f meetup.jq << 'END_INPUT'
        {
          "year": 2013,
          "month": 3,
          "week": "second",
          "dayofweek": "Sunday"
        }
END_INPUT

    assert_success
    expected='2013-03-10'
    assert_equal "$output" "$expected"
}

@test 'when second Sunday is the 14th, the last day of the second week' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f meetup.jq << 'END_INPUT'
        {
          "year": 2013,
          "month": 4,
          "week": "second",
          "dayofweek": "Sunday"
        }
END_INPUT

    assert_success
    expected='2013-04-14'
    assert_equal "$output" "$expected"
}

@test 'when third Monday is some day in the middle of the third week' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f meetup.jq << 'END_INPUT'
        {
          "year": 2013,
          "month": 3,
          "week": "third",
          "dayofweek": "Monday"
        }
END_INPUT

    assert_success
    expected='2013-03-18'
    assert_equal "$output" "$expected"
}

@test 'when third Monday is the 15th, the first day of the third week' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f meetup.jq << 'END_INPUT'
        {
          "year": 2013,
          "month": 4,
          "week": "third",
          "dayofweek": "Monday"
        }
END_INPUT

    assert_success
    expected='2013-04-15'
    assert_equal "$output" "$expected"
}

@test 'when third Tuesday is the 21st, the last day of the third week' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f meetup.jq << 'END_INPUT'
        {
          "year": 2013,
          "month": 5,
          "week": "third",
          "dayofweek": "Tuesday"
        }
END_INPUT

    assert_success
    expected='2013-05-21'
    assert_equal "$output" "$expected"
}

@test 'when third Tuesday is some day in the middle of the third week' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f meetup.jq << 'END_INPUT'
        {
          "year": 2013,
          "month": 6,
          "week": "third",
          "dayofweek": "Tuesday"
        }
END_INPUT

    assert_success
    expected='2013-06-18'
    assert_equal "$output" "$expected"
}

@test 'when third Wednesday is some day in the middle of the third week' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f meetup.jq << 'END_INPUT'
        {
          "year": 2013,
          "month": 7,
          "week": "third",
          "dayofweek": "Wednesday"
        }
END_INPUT

    assert_success
    expected='2013-07-17'
    assert_equal "$output" "$expected"
}

@test 'when third Wednesday is the 21st, the last day of the third week' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f meetup.jq << 'END_INPUT'
        {
          "year": 2013,
          "month": 8,
          "week": "third",
          "dayofweek": "Wednesday"
        }
END_INPUT

    assert_success
    expected='2013-08-21'
    assert_equal "$output" "$expected"
}

@test 'when third Thursday is some day in the middle of the third week' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f meetup.jq << 'END_INPUT'
        {
          "year": 2013,
          "month": 9,
          "week": "third",
          "dayofweek": "Thursday"
        }
END_INPUT

    assert_success
    expected='2013-09-19'
    assert_equal "$output" "$expected"
}

@test 'when third Thursday is another day in the middle of the third week' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f meetup.jq << 'END_INPUT'
        {
          "year": 2013,
          "month": 10,
          "week": "third",
          "dayofweek": "Thursday"
        }
END_INPUT

    assert_success
    expected='2013-10-17'
    assert_equal "$output" "$expected"
}

@test 'when third Friday is the 15th, the first day of the third week' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f meetup.jq << 'END_INPUT'
        {
          "year": 2013,
          "month": 11,
          "week": "third",
          "dayofweek": "Friday"
        }
END_INPUT

    assert_success
    expected='2013-11-15'
    assert_equal "$output" "$expected"
}

@test 'when third Friday is some day in the middle of the third week' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f meetup.jq << 'END_INPUT'
        {
          "year": 2013,
          "month": 12,
          "week": "third",
          "dayofweek": "Friday"
        }
END_INPUT

    assert_success
    expected='2013-12-20'
    assert_equal "$output" "$expected"
}

@test 'when third Saturday is some day in the middle of the third week' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f meetup.jq << 'END_INPUT'
        {
          "year": 2013,
          "month": 1,
          "week": "third",
          "dayofweek": "Saturday"
        }
END_INPUT

    assert_success
    expected='2013-01-19'
    assert_equal "$output" "$expected"
}

@test 'when third Saturday is another day in the middle of the third week' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f meetup.jq << 'END_INPUT'
        {
          "year": 2013,
          "month": 2,
          "week": "third",
          "dayofweek": "Saturday"
        }
END_INPUT

    assert_success
    expected='2013-02-16'
    assert_equal "$output" "$expected"
}

@test 'when third Sunday is some day in the middle of the third week' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f meetup.jq << 'END_INPUT'
        {
          "year": 2013,
          "month": 3,
          "week": "third",
          "dayofweek": "Sunday"
        }
END_INPUT

    assert_success
    expected='2013-03-17'
    assert_equal "$output" "$expected"
}

@test 'when third Sunday is the 21st, the last day of the third week' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f meetup.jq << 'END_INPUT'
        {
          "year": 2013,
          "month": 4,
          "week": "third",
          "dayofweek": "Sunday"
        }
END_INPUT

    assert_success
    expected='2013-04-21'
    assert_equal "$output" "$expected"
}

@test 'when fourth Monday is some day in the middle of the fourth week' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f meetup.jq << 'END_INPUT'
        {
          "year": 2013,
          "month": 3,
          "week": "fourth",
          "dayofweek": "Monday"
        }
END_INPUT

    assert_success
    expected='2013-03-25'
    assert_equal "$output" "$expected"
}

@test 'when fourth Monday is the 22nd, the first day of the fourth week' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f meetup.jq << 'END_INPUT'
        {
          "year": 2013,
          "month": 4,
          "week": "fourth",
          "dayofweek": "Monday"
        }
END_INPUT

    assert_success
    expected='2013-04-22'
    assert_equal "$output" "$expected"
}

@test 'when fourth Tuesday is the 28th, the last day of the fourth week' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f meetup.jq << 'END_INPUT'
        {
          "year": 2013,
          "month": 5,
          "week": "fourth",
          "dayofweek": "Tuesday"
        }
END_INPUT

    assert_success
    expected='2013-05-28'
    assert_equal "$output" "$expected"
}

@test 'when fourth Tuesday is some day in the middle of the fourth week' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f meetup.jq << 'END_INPUT'
        {
          "year": 2013,
          "month": 6,
          "week": "fourth",
          "dayofweek": "Tuesday"
        }
END_INPUT

    assert_success
    expected='2013-06-25'
    assert_equal "$output" "$expected"
}

@test 'when fourth Wednesday is some day in the middle of the fourth week' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f meetup.jq << 'END_INPUT'
        {
          "year": 2013,
          "month": 7,
          "week": "fourth",
          "dayofweek": "Wednesday"
        }
END_INPUT

    assert_success
    expected='2013-07-24'
    assert_equal "$output" "$expected"
}

@test 'when fourth Wednesday is the 28th, the last day of the fourth week' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f meetup.jq << 'END_INPUT'
        {
          "year": 2013,
          "month": 8,
          "week": "fourth",
          "dayofweek": "Wednesday"
        }
END_INPUT

    assert_success
    expected='2013-08-28'
    assert_equal "$output" "$expected"
}

@test 'when fourth Thursday is some day in the middle of the fourth week' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f meetup.jq << 'END_INPUT'
        {
          "year": 2013,
          "month": 9,
          "week": "fourth",
          "dayofweek": "Thursday"
        }
END_INPUT

    assert_success
    expected='2013-09-26'
    assert_equal "$output" "$expected"
}

@test 'when fourth Thursday is another day in the middle of the fourth week' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f meetup.jq << 'END_INPUT'
        {
          "year": 2013,
          "month": 10,
          "week": "fourth",
          "dayofweek": "Thursday"
        }
END_INPUT

    assert_success
    expected='2013-10-24'
    assert_equal "$output" "$expected"
}

@test 'when fourth Friday is the 22nd, the first day of the fourth week' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f meetup.jq << 'END_INPUT'
        {
          "year": 2013,
          "month": 11,
          "week": "fourth",
          "dayofweek": "Friday"
        }
END_INPUT

    assert_success
    expected='2013-11-22'
    assert_equal "$output" "$expected"
}

@test 'when fourth Friday is some day in the middle of the fourth week' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f meetup.jq << 'END_INPUT'
        {
          "year": 2013,
          "month": 12,
          "week": "fourth",
          "dayofweek": "Friday"
        }
END_INPUT

    assert_success
    expected='2013-12-27'
    assert_equal "$output" "$expected"
}

@test 'when fourth Saturday is some day in the middle of the fourth week' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f meetup.jq << 'END_INPUT'
        {
          "year": 2013,
          "month": 1,
          "week": "fourth",
          "dayofweek": "Saturday"
        }
END_INPUT

    assert_success
    expected='2013-01-26'
    assert_equal "$output" "$expected"
}

@test 'when fourth Saturday is another day in the middle of the fourth week' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f meetup.jq << 'END_INPUT'
        {
          "year": 2013,
          "month": 2,
          "week": "fourth",
          "dayofweek": "Saturday"
        }
END_INPUT

    assert_success
    expected='2013-02-23'
    assert_equal "$output" "$expected"
}

@test 'when fourth Sunday is some day in the middle of the fourth week' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f meetup.jq << 'END_INPUT'
        {
          "year": 2013,
          "month": 3,
          "week": "fourth",
          "dayofweek": "Sunday"
        }
END_INPUT

    assert_success
    expected='2013-03-24'
    assert_equal "$output" "$expected"
}

@test 'when fourth Sunday is the 28th, the last day of the fourth week' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f meetup.jq << 'END_INPUT'
        {
          "year": 2013,
          "month": 4,
          "week": "fourth",
          "dayofweek": "Sunday"
        }
END_INPUT

    assert_success
    expected='2013-04-28'
    assert_equal "$output" "$expected"
}

@test 'last Monday in a month with four Mondays' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f meetup.jq << 'END_INPUT'
        {
          "year": 2013,
          "month": 3,
          "week": "last",
          "dayofweek": "Monday"
        }
END_INPUT

    assert_success
    expected='2013-03-25'
    assert_equal "$output" "$expected"
}

@test 'last Monday in a month with five Mondays' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f meetup.jq << 'END_INPUT'
        {
          "year": 2013,
          "month": 4,
          "week": "last",
          "dayofweek": "Monday"
        }
END_INPUT

    assert_success
    expected='2013-04-29'
    assert_equal "$output" "$expected"
}

@test 'last Tuesday in a month with four Tuesdays' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f meetup.jq << 'END_INPUT'
        {
          "year": 2013,
          "month": 5,
          "week": "last",
          "dayofweek": "Tuesday"
        }
END_INPUT

    assert_success
    expected='2013-05-28'
    assert_equal "$output" "$expected"
}

@test 'last Tuesday in another month with four Tuesdays' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f meetup.jq << 'END_INPUT'
        {
          "year": 2013,
          "month": 6,
          "week": "last",
          "dayofweek": "Tuesday"
        }
END_INPUT

    assert_success
    expected='2013-06-25'
    assert_equal "$output" "$expected"
}

@test 'last Wednesday in a month with five Wednesdays' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f meetup.jq << 'END_INPUT'
        {
          "year": 2013,
          "month": 7,
          "week": "last",
          "dayofweek": "Wednesday"
        }
END_INPUT

    assert_success
    expected='2013-07-31'
    assert_equal "$output" "$expected"
}

@test 'last Wednesday in a month with four Wednesdays' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f meetup.jq << 'END_INPUT'
        {
          "year": 2013,
          "month": 8,
          "week": "last",
          "dayofweek": "Wednesday"
        }
END_INPUT

    assert_success
    expected='2013-08-28'
    assert_equal "$output" "$expected"
}

@test 'last Thursday in a month with four Thursdays' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f meetup.jq << 'END_INPUT'
        {
          "year": 2013,
          "month": 9,
          "week": "last",
          "dayofweek": "Thursday"
        }
END_INPUT

    assert_success
    expected='2013-09-26'
    assert_equal "$output" "$expected"
}

@test 'last Thursday in a month with five Thursdays' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f meetup.jq << 'END_INPUT'
        {
          "year": 2013,
          "month": 10,
          "week": "last",
          "dayofweek": "Thursday"
        }
END_INPUT

    assert_success
    expected='2013-10-31'
    assert_equal "$output" "$expected"
}

@test 'last Friday in a month with five Fridays' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f meetup.jq << 'END_INPUT'
        {
          "year": 2013,
          "month": 11,
          "week": "last",
          "dayofweek": "Friday"
        }
END_INPUT

    assert_success
    expected='2013-11-29'
    assert_equal "$output" "$expected"
}

@test 'last Friday in a month with four Fridays' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f meetup.jq << 'END_INPUT'
        {
          "year": 2013,
          "month": 12,
          "week": "last",
          "dayofweek": "Friday"
        }
END_INPUT

    assert_success
    expected='2013-12-27'
    assert_equal "$output" "$expected"
}

@test 'last Saturday in a month with four Saturdays' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f meetup.jq << 'END_INPUT'
        {
          "year": 2013,
          "month": 1,
          "week": "last",
          "dayofweek": "Saturday"
        }
END_INPUT

    assert_success
    expected='2013-01-26'
    assert_equal "$output" "$expected"
}

@test 'last Saturday in another month with four Saturdays' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f meetup.jq << 'END_INPUT'
        {
          "year": 2013,
          "month": 2,
          "week": "last",
          "dayofweek": "Saturday"
        }
END_INPUT

    assert_success
    expected='2013-02-23'
    assert_equal "$output" "$expected"
}

@test 'last Sunday in a month with five Sundays' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f meetup.jq << 'END_INPUT'
        {
          "year": 2013,
          "month": 3,
          "week": "last",
          "dayofweek": "Sunday"
        }
END_INPUT

    assert_success
    expected='2013-03-31'
    assert_equal "$output" "$expected"
}

@test 'last Sunday in a month with four Sundays' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f meetup.jq << 'END_INPUT'
        {
          "year": 2013,
          "month": 4,
          "week": "last",
          "dayofweek": "Sunday"
        }
END_INPUT

    assert_success
    expected='2013-04-28'
    assert_equal "$output" "$expected"
}

@test 'when last Wednesday in February in a leap year is the 29th' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f meetup.jq << 'END_INPUT'
        {
          "year": 2012,
          "month": 2,
          "week": "last",
          "dayofweek": "Wednesday"
        }
END_INPUT

    assert_success
    expected='2012-02-29'
    assert_equal "$output" "$expected"
}

@test 'last Wednesday in December that is also the last day of the year' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f meetup.jq << 'END_INPUT'
        {
          "year": 2014,
          "month": 12,
          "week": "last",
          "dayofweek": "Wednesday"
        }
END_INPUT

    assert_success
    expected='2014-12-31'
    assert_equal "$output" "$expected"
}

@test 'when last Sunday in February in a non-leap year is not the 29th' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f meetup.jq << 'END_INPUT'
        {
          "year": 2015,
          "month": 2,
          "week": "last",
          "dayofweek": "Sunday"
        }
END_INPUT

    assert_success
    expected='2015-02-22'
    assert_equal "$output" "$expected"
}

@test 'when first Friday is the 7th, the last day of the first week' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f meetup.jq << 'END_INPUT'
        {
          "year": 2012,
          "month": 12,
          "week": "first",
          "dayofweek": "Friday"
        }
END_INPUT

    assert_success
    expected='2012-12-07'
    assert_equal "$output" "$expected"
}

