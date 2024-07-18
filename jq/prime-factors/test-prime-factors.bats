#!/usr/bin/env bats
# generated on 2024-07-12T18:41:13Z
load bats-extra
load bats-jq

@test 'no factors' {
    #[[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f prime-factors.jq << 'END_INPUT'
        {
          "value": 1
        }
END_INPUT

    assert_success
    expected='[]'
    assert_equal "$output" "$expected"
}

@test 'prime number' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f prime-factors.jq << 'END_INPUT'
        {
          "value": 2
        }
END_INPUT

    assert_success
    expected='[2]'
    assert_equal "$output" "$expected"
}

@test 'another prime number' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f prime-factors.jq << 'END_INPUT'
        {
          "value": 3
        }
END_INPUT

    assert_success
    expected='[3]'
    assert_equal "$output" "$expected"
}

@test 'square of a prime' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f prime-factors.jq << 'END_INPUT'
        {
          "value": 9
        }
END_INPUT

    assert_success
    expected='[3,3]'
    assert_equal "$output" "$expected"
}

@test 'product of first prime' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f prime-factors.jq << 'END_INPUT'
        {
          "value": 4
        }
END_INPUT

    assert_success
    expected='[2,2]'
    assert_equal "$output" "$expected"
}

@test 'cube of a prime' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f prime-factors.jq << 'END_INPUT'
        {
          "value": 8
        }
END_INPUT

    assert_success
    expected='[2,2,2]'
    assert_equal "$output" "$expected"
}

@test 'product of second prime' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f prime-factors.jq << 'END_INPUT'
        {
          "value": 27
        }
END_INPUT

    assert_success
    expected='[3,3,3]'
    assert_equal "$output" "$expected"
}

@test 'product of third prime' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f prime-factors.jq << 'END_INPUT'
        {
          "value": 625
        }
END_INPUT

    assert_success
    expected='[5,5,5,5]'
    assert_equal "$output" "$expected"
}

@test 'product of first and second prime' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f prime-factors.jq << 'END_INPUT'
        {
          "value": 6
        }
END_INPUT

    assert_success
    expected='[2,3]'
    assert_equal "$output" "$expected"
}

@test 'product of primes and non-primes' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f prime-factors.jq << 'END_INPUT'
        {
          "value": 12
        }
END_INPUT

    assert_success
    expected='[2,2,3]'
    assert_equal "$output" "$expected"
}

@test 'product of primes' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f prime-factors.jq << 'END_INPUT'
        {
          "value": 901255
        }
END_INPUT

    assert_success
    expected='[5,17,23,461]'
    assert_equal "$output" "$expected"
}

@test 'factors include a large prime' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f prime-factors.jq << 'END_INPUT'
        {
          "value": 93819012551
        }
END_INPUT

    assert_success
    expected='[11,9539,894119]'
    assert_equal "$output" "$expected"
}
