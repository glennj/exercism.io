#!/usr/bin/env bats
# generated on 2022-11-02T20:59:03Z
load bats-extra
load bats-jq

@test 'zero steps for one' {
    #[[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r 'import "./collatz-conjecture" as Collatz; .number | Collatz::steps' << 'END_INPUT'
        {
          "number": 1
        }
END_INPUT

    assert_success
    expected=0
    assert_equal "$output" "$expected"
}

@test 'divide if even' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r 'import "./collatz-conjecture" as Collatz; .number | Collatz::steps' << 'END_INPUT'
        {
          "number": 16
        }
END_INPUT

    assert_success
    expected=4
    assert_equal "$output" "$expected"
}

@test 'even and odd steps' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r 'import "./collatz-conjecture" as Collatz; .number | Collatz::steps' << 'END_INPUT'
        {
          "number": 12
        }
END_INPUT

    assert_success
    expected=9
    assert_equal "$output" "$expected"
}

@test 'large number of even and odd steps' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r 'import "./collatz-conjecture" as Collatz; .number | Collatz::steps' << 'END_INPUT'
        {
          "number": 1000000
        }
END_INPUT

    assert_success
    expected=152
    assert_equal "$output" "$expected"
}

@test 'zero is an error' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r 'import "./collatz-conjecture" as Collatz; .number | Collatz::steps' << 'END_INPUT'
        {
          "number": 0
        }
END_INPUT

    assert_failure
    expected='Only positive integers are allowed'
    assert_equal "$output" "$expected"
}

@test 'negative value is an error' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r 'import "./collatz-conjecture" as Collatz; .number | Collatz::steps' << 'END_INPUT'
        {
          "number": -15
        }
END_INPUT

    assert_failure
    expected='Only positive integers are allowed'
    assert_equal "$output" "$expected"
}

