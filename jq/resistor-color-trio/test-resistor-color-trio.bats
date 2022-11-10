#!/usr/bin/env bats
# generated on 2022-11-02T20:59:41Z
load bats-extra

@test 'Orange and orange and black' {
    #[[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f resistor-color-trio.jq << 'END_INPUT'
        {
          "colors": [
            "orange",
            "orange",
            "black"
          ]
        }
END_INPUT

    assert_success
    expected='{"value":33,"unit":"ohms"}'
    assert_equal "$output" "$expected"
}

@test 'Blue and grey and brown' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f resistor-color-trio.jq << 'END_INPUT'
        {
          "colors": [
            "blue",
            "grey",
            "brown"
          ]
        }
END_INPUT

    assert_success
    expected='{"value":680,"unit":"ohms"}'
    assert_equal "$output" "$expected"
}

@test 'Red and black and red' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f resistor-color-trio.jq << 'END_INPUT'
        {
          "colors": [
            "red",
            "black",
            "red"
          ]
        }
END_INPUT

    assert_success
    expected='{"value":2,"unit":"kiloohms"}'
    assert_equal "$output" "$expected"
}

@test 'Green and brown and orange' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f resistor-color-trio.jq << 'END_INPUT'
        {
          "colors": [
            "green",
            "brown",
            "orange"
          ]
        }
END_INPUT

    assert_success
    expected='{"value":51,"unit":"kiloohms"}'
    assert_equal "$output" "$expected"
}

@test 'Yellow and violet and yellow' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f resistor-color-trio.jq << 'END_INPUT'
        {
          "colors": [
            "yellow",
            "violet",
            "yellow"
          ]
        }
END_INPUT

    assert_success
    expected='{"value":470,"unit":"kiloohms"}'
    assert_equal "$output" "$expected"
}

