#!/usr/bin/env bats
# generated on 2024-07-12T20:38:30Z
load bats-extra
load bats-jq

@test 'cleans the number' {
    #[[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f phone-number.jq << 'END_INPUT'
        {
          "phrase": "(223) 456-7890"
        }
END_INPUT

    assert_success
    expected='2234567890'
    assert_equal "$output" "$expected"
}

@test 'cleans numbers with dots' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f phone-number.jq << 'END_INPUT'
        {
          "phrase": "223.456.7890"
        }
END_INPUT

    assert_success
    expected='2234567890'
    assert_equal "$output" "$expected"
}

@test 'cleans numbers with multiple spaces' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f phone-number.jq << 'END_INPUT'
        {
          "phrase": "223 456   7890   "
        }
END_INPUT

    assert_success
    expected='2234567890'
    assert_equal "$output" "$expected"
}

@test 'invalid when 9 digits' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f phone-number.jq << 'END_INPUT'
        {
          "phrase": "123456789"
        }
END_INPUT

    assert_failure
    expected='must not be fewer than 10 digits'
    assert_equal "$output" "$expected"
}

@test 'invalid when 11 digits does not start with a 1' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f phone-number.jq << 'END_INPUT'
        {
          "phrase": "22234567890"
        }
END_INPUT

    assert_failure
    expected='11 digits must start with 1'
    assert_equal "$output" "$expected"
}

@test 'valid when 11 digits and starting with 1' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f phone-number.jq << 'END_INPUT'
        {
          "phrase": "12234567890"
        }
END_INPUT

    assert_success
    expected='2234567890'
    assert_equal "$output" "$expected"
}

@test 'valid when 11 digits and starting with 1 even with punctuation' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f phone-number.jq << 'END_INPUT'
        {
          "phrase": "+1 (223) 456-7890"
        }
END_INPUT

    assert_success
    expected='2234567890'
    assert_equal "$output" "$expected"
}

@test 'invalid when more than 11 digits' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f phone-number.jq << 'END_INPUT'
        {
          "phrase": "321234567890"
        }
END_INPUT

    assert_failure
    expected='must not be greater than 11 digits'
    assert_equal "$output" "$expected"
}

@test 'invalid with letters' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f phone-number.jq << 'END_INPUT'
        {
          "phrase": "523-abc-7890"
        }
END_INPUT

    assert_failure
    expected='letters not permitted'
    assert_equal "$output" "$expected"
}

@test 'invalid with punctuations' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f phone-number.jq << 'END_INPUT'
        {
          "phrase": "523-@:!-7890"
        }
END_INPUT

    assert_failure
    expected='punctuations not permitted'
    assert_equal "$output" "$expected"
}

@test 'invalid if area code starts with 0' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f phone-number.jq << 'END_INPUT'
        {
          "phrase": "(023) 456-7890"
        }
END_INPUT

    assert_failure
    expected='area code cannot start with zero'
    assert_equal "$output" "$expected"
}

@test 'invalid if area code starts with 1' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f phone-number.jq << 'END_INPUT'
        {
          "phrase": "(123) 456-7890"
        }
END_INPUT

    assert_failure
    expected='area code cannot start with one'
    assert_equal "$output" "$expected"
}

@test 'invalid if exchange code starts with 0' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f phone-number.jq << 'END_INPUT'
        {
          "phrase": "(223) 056-7890"
        }
END_INPUT

    assert_failure
    expected='exchange code cannot start with zero'
    assert_equal "$output" "$expected"
}

@test 'invalid if exchange code starts with 1' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f phone-number.jq << 'END_INPUT'
        {
          "phrase": "(223) 156-7890"
        }
END_INPUT

    assert_failure
    expected='exchange code cannot start with one'
    assert_equal "$output" "$expected"
}

@test 'invalid if area code starts with 0 on valid 11-digit number' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f phone-number.jq << 'END_INPUT'
        {
          "phrase": "1 (023) 456-7890"
        }
END_INPUT

    assert_failure
    expected='area code cannot start with zero'
    assert_equal "$output" "$expected"
}

@test 'invalid if area code starts with 1 on valid 11-digit number' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f phone-number.jq << 'END_INPUT'
        {
          "phrase": "1 (123) 456-7890"
        }
END_INPUT

    assert_failure
    expected='area code cannot start with one'
    assert_equal "$output" "$expected"
}

@test 'invalid if exchange code starts with 0 on valid 11-digit number' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f phone-number.jq << 'END_INPUT'
        {
          "phrase": "1 (223) 056-7890"
        }
END_INPUT

    assert_failure
    expected='exchange code cannot start with zero'
    assert_equal "$output" "$expected"
}

@test 'invalid if exchange code starts with 1 on valid 11-digit number' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f phone-number.jq << 'END_INPUT'
        {
          "phrase": "1 (223) 156-7890"
        }
END_INPUT

    assert_failure
    expected='exchange code cannot start with one'
    assert_equal "$output" "$expected"
}
