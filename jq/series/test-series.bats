#!/usr/bin/env bats
# generated on 2023-08-25T15:56:38Z
load bats-extra

@test 'slices of one from one' {
    #[[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f series.jq << 'END_INPUT'
{
  "series": "1",
  "sliceLength": 1
}
END_INPUT

    assert_success
    expected=$(cat << 'END_EXPECTED'
[
  "1"
]
END_EXPECTED
)
    assert_equal "$output" "$expected"
}

@test 'slices of one from two' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f series.jq << 'END_INPUT'
{
  "series": "12",
  "sliceLength": 1
}
END_INPUT

    assert_success
    expected=$(cat << 'END_EXPECTED'
[
  "1",
  "2"
]
END_EXPECTED
)
    assert_equal "$output" "$expected"
}

@test 'slices of two' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f series.jq << 'END_INPUT'
{
  "series": "35",
  "sliceLength": 2
}
END_INPUT

    assert_success
    expected=$(cat << 'END_EXPECTED'
[
  "35"
]
END_EXPECTED
)
    assert_equal "$output" "$expected"
}

@test 'slices of two overlap' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f series.jq << 'END_INPUT'
{
  "series": "9142",
  "sliceLength": 2
}
END_INPUT

    assert_success
    expected=$(cat << 'END_EXPECTED'
[
  "91",
  "14",
  "42"
]
END_EXPECTED
)
    assert_equal "$output" "$expected"
}

@test 'slices can include duplicates' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f series.jq << 'END_INPUT'
{
  "series": "777777",
  "sliceLength": 3
}
END_INPUT

    assert_success
    expected=$(cat << 'END_EXPECTED'
[
  "777",
  "777",
  "777",
  "777"
]
END_EXPECTED
)
    assert_equal "$output" "$expected"
}

@test 'slices of a long series' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f series.jq << 'END_INPUT'
{
  "series": "918493904243",
  "sliceLength": 5
}
END_INPUT

    assert_success
    expected=$(cat << 'END_EXPECTED'
[
  "91849",
  "18493",
  "84939",
  "49390",
  "93904",
  "39042",
  "90424",
  "04243"
]
END_EXPECTED
)
    assert_equal "$output" "$expected"
}

@test 'slice length is too large' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f series.jq << 'END_INPUT'
{
  "series": "12345",
  "sliceLength": 6
}
END_INPUT

    assert_failure
    expected='slice length cannot be greater than series length'

    assert_equal "$output" "$expected"
}

@test 'slice length is way too large' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f series.jq << 'END_INPUT'
{
  "series": "12345",
  "sliceLength": 42
}
END_INPUT

    assert_failure
    expected='slice length cannot be greater than series length'

    assert_equal "$output" "$expected"
}

@test 'slice length cannot be zero' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f series.jq << 'END_INPUT'
{
  "series": "12345",
  "sliceLength": 0
}
END_INPUT

    assert_failure
    expected='slice length cannot be zero'

    assert_equal "$output" "$expected"
}

@test 'slice length cannot be negative' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f series.jq << 'END_INPUT'
{
  "series": "123",
  "sliceLength": -1
}
END_INPUT

    assert_failure
    expected='slice length cannot be negative'

    assert_equal "$output" "$expected"
}

@test 'empty series is invalid' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f series.jq << 'END_INPUT'
{
  "series": "",
  "sliceLength": 1
}
END_INPUT

    assert_failure
    expected='series cannot be empty'

    assert_equal "$output" "$expected"
}
