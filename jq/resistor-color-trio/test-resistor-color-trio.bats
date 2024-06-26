#!/usr/bin/env bats
# generated on 2023-01-03T20:57:23Z
load bats-extra
load bats-jq

@test 'Orange and orange and black' {
    #[[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f resistor-color-trio.jq << 'END_INPUT'
{
  "colors": [
    "orange",
    "orange",
    "black"
  ]
}
END_INPUT

    assert_success
    expected=$(cat << 'END_EXPECTED'
{
  "value": 33,
  "unit": "ohms"
}
END_EXPECTED
)
    assert_equal "$output" "$expected"
}

@test 'Blue and grey and brown' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f resistor-color-trio.jq << 'END_INPUT'
{
  "colors": [
    "blue",
    "grey",
    "brown"
  ]
}
END_INPUT

    assert_success
    expected=$(cat << 'END_EXPECTED'
{
  "value": 680,
  "unit": "ohms"
}
END_EXPECTED
)
    assert_equal "$output" "$expected"
}

@test 'Red and black and red' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f resistor-color-trio.jq << 'END_INPUT'
{
  "colors": [
    "red",
    "black",
    "red"
  ]
}
END_INPUT

    assert_success
    expected=$(cat << 'END_EXPECTED'
{
  "value": 2,
  "unit": "kiloohms"
}
END_EXPECTED
)
    assert_equal "$output" "$expected"
}

@test 'Green and brown and orange' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f resistor-color-trio.jq << 'END_INPUT'
{
  "colors": [
    "green",
    "brown",
    "orange"
  ]
}
END_INPUT

    assert_success
    expected=$(cat << 'END_EXPECTED'
{
  "value": 51,
  "unit": "kiloohms"
}
END_EXPECTED
)
    assert_equal "$output" "$expected"
}

@test 'Yellow and violet and yellow' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f resistor-color-trio.jq << 'END_INPUT'
{
  "colors": [
    "yellow",
    "violet",
    "yellow"
  ]
}
END_INPUT

    assert_success
    expected=$(cat << 'END_EXPECTED'
{
  "value": 470,
  "unit": "kiloohms"
}
END_EXPECTED
)
    assert_equal "$output" "$expected"
}

@test 'Blue and violet and blue' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f resistor-color-trio.jq << 'END_INPUT'
{
  "colors": [
    "blue",
    "violet",
    "blue"
  ]
}
END_INPUT

    assert_success
    expected=$(cat << 'END_EXPECTED'
{
  "value": 67,
  "unit": "megaohms"
}
END_EXPECTED
)
    assert_equal "$output" "$expected"
}

@test 'Minimum possible value' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f resistor-color-trio.jq << 'END_INPUT'
{
  "colors": [
    "black",
    "black",
    "black"
  ]
}
END_INPUT

    assert_success
    expected=$(cat << 'END_EXPECTED'
{
  "value": 0,
  "unit": "ohms"
}
END_EXPECTED
)
    assert_equal "$output" "$expected"
}

@test 'Maximum possible value' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f resistor-color-trio.jq << 'END_INPUT'
{
  "colors": [
    "white",
    "white",
    "white"
  ]
}
END_INPUT

    assert_success
    expected=$(cat << 'END_EXPECTED'
{
  "value": 99,
  "unit": "gigaohms"
}
END_EXPECTED
)
    assert_equal "$output" "$expected"
}

@test 'First two colors make an invalid octal number' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f resistor-color-trio.jq << 'END_INPUT'
{
  "colors": [
    "black",
    "grey",
    "black"
  ]
}
END_INPUT

    assert_success
    expected=$(cat << 'END_EXPECTED'
{
  "value": 8,
  "unit": "ohms"
}
END_EXPECTED
)
    assert_equal "$output" "$expected"
}

@test 'Ignore extra colors' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f resistor-color-trio.jq << 'END_INPUT'
{
  "colors": [
    "blue",
    "green",
    "yellow",
    "orange"
  ]
}
END_INPUT

    assert_success
    expected=$(cat << 'END_EXPECTED'
{
  "value": 650,
  "unit": "kiloohms"
}
END_EXPECTED
)
    assert_equal "$output" "$expected"
}
