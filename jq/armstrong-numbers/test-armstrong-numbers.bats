#!/usr/bin/env bats
# generated on 2024-06-14T21:01:19Z
load bats-extra

@test 'Zero is an Armstrong number' {
    #[[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f armstrong-numbers.jq << 'END_INPUT'
        {
          "number": 0
        }
END_INPUT

    assert_success
    expected=true
    assert_equal "$output" "$expected"
}

@test 'Single-digit numbers are Armstrong numbers' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f armstrong-numbers.jq << 'END_INPUT'
        {
          "number": 5
        }
END_INPUT

    assert_success
    expected=true
    assert_equal "$output" "$expected"
}

@test 'There are no two-digit Armstrong numbers' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f armstrong-numbers.jq << 'END_INPUT'
        {
          "number": 10
        }
END_INPUT

    assert_success
    expected=false
    assert_equal "$output" "$expected"
}

@test 'Three-digit number that is an Armstrong number' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f armstrong-numbers.jq << 'END_INPUT'
        {
          "number": 153
        }
END_INPUT

    assert_success
    expected=true
    assert_equal "$output" "$expected"
}

@test 'Three-digit number that is not an Armstrong number' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f armstrong-numbers.jq << 'END_INPUT'
        {
          "number": 100
        }
END_INPUT

    assert_success
    expected=false
    assert_equal "$output" "$expected"
}

@test 'Four-digit number that is an Armstrong number' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f armstrong-numbers.jq << 'END_INPUT'
        {
          "number": 9474
        }
END_INPUT

    assert_success
    expected=true
    assert_equal "$output" "$expected"
}

@test 'Four-digit number that is not an Armstrong number' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f armstrong-numbers.jq << 'END_INPUT'
        {
          "number": 9475
        }
END_INPUT

    assert_success
    expected=false
    assert_equal "$output" "$expected"
}

@test 'Seven-digit number that is an Armstrong number' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f armstrong-numbers.jq << 'END_INPUT'
        {
          "number": 9926315
        }
END_INPUT

    assert_success
    expected=true
    assert_equal "$output" "$expected"
}

@test 'Seven-digit number that is not an Armstrong number' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f armstrong-numbers.jq << 'END_INPUT'
        {
          "number": 9926314
        }
END_INPUT

    assert_success
    expected=false
    assert_equal "$output" "$expected"
}
