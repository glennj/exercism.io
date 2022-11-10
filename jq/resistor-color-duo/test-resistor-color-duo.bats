#!/usr/bin/env bats
# generated on 2022-11-02T20:59:40Z
load bats-extra

@test 'Brown and black' {
    #[[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f resistor-color-duo.jq << 'END_INPUT'
        {
          "colors": [
            "brown",
            "black"
          ]
        }
END_INPUT

    assert_success
    expected=10
    assert_equal "$output" "$expected"
}

@test 'Blue and grey' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f resistor-color-duo.jq << 'END_INPUT'
        {
          "colors": [
            "blue",
            "grey"
          ]
        }
END_INPUT

    assert_success
    expected=68
    assert_equal "$output" "$expected"
}

@test 'Yellow and violet' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f resistor-color-duo.jq << 'END_INPUT'
        {
          "colors": [
            "yellow",
            "violet"
          ]
        }
END_INPUT

    assert_success
    expected=47
    assert_equal "$output" "$expected"
}

@test 'White and red' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f resistor-color-duo.jq << 'END_INPUT'
        {
          "colors": [
            "white",
            "red"
          ]
        }
END_INPUT

    assert_success
    expected=92
    assert_equal "$output" "$expected"
}

@test 'Orange and orange' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f resistor-color-duo.jq << 'END_INPUT'
        {
          "colors": [
            "orange",
            "orange"
          ]
        }
END_INPUT

    assert_success
    expected=33
    assert_equal "$output" "$expected"
}

@test 'Ignore additional colors' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f resistor-color-duo.jq << 'END_INPUT'
        {
          "colors": [
            "green",
            "brown",
            "orange"
          ]
        }
END_INPUT

    assert_success
    expected=51
    assert_equal "$output" "$expected"
}

@test 'Black and brown, one-digit' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f resistor-color-duo.jq << 'END_INPUT'
        {
          "colors": [
            "black",
            "brown"
          ]
        }
END_INPUT

    assert_success
    expected=1
    assert_equal "$output" "$expected"
}

