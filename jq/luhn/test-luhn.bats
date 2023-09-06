#!/usr/bin/env bats
# generated on 2023-08-25T13:21:10Z
load bats-extra

@test 'single digit strings can not be valid' {
    #[[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f luhn.jq <<< '"1"'

    assert_success
    expected=false
    assert_equal "$output" "$expected"
}

@test 'a single zero is invalid' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f luhn.jq <<< '"0"'

    assert_success
    expected=false
    assert_equal "$output" "$expected"
}

@test 'a simple valid SIN that remains valid if reversed' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f luhn.jq <<< '"059"'

    assert_success
    expected=true
    assert_equal "$output" "$expected"
}

@test 'a simple valid SIN that becomes invalid if reversed' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f luhn.jq <<< '"59"'

    assert_success
    expected=true
    assert_equal "$output" "$expected"
}

@test 'a valid Canadian SIN' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f luhn.jq <<< '"055 444 285"'

    assert_success
    expected=true
    assert_equal "$output" "$expected"
}

@test 'invalid Canadian SIN' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f luhn.jq <<< '"055 444 286"'

    assert_success
    expected=false
    assert_equal "$output" "$expected"
}

@test 'invalid credit card' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f luhn.jq <<< '"8273 1232 7352 0569"'

    assert_success
    expected=false
    assert_equal "$output" "$expected"
}

@test 'invalid long number with an even remainder' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f luhn.jq <<< '"1 2345 6789 1234 5678 9012"'

    assert_success
    expected=false
    assert_equal "$output" "$expected"
}

@test 'invalid long number with a remainder divisible by 5' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f luhn.jq <<< '"1 2345 6789 1234 5678 9013"'

    assert_success
    expected=false
    assert_equal "$output" "$expected"
}

@test 'valid number with an even number of digits' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f luhn.jq <<< '"095 245 88"'

    assert_success
    expected=true
    assert_equal "$output" "$expected"
}

@test 'valid number with an odd number of spaces' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f luhn.jq <<< '"234 567 891 234"'

    assert_success
    expected=true
    assert_equal "$output" "$expected"
}

@test 'valid strings with a non-digit added at the end become invalid' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f luhn.jq <<< '"059a"'

    assert_success
    expected=false
    assert_equal "$output" "$expected"
}

@test 'valid strings with punctuation included become invalid' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f luhn.jq <<< '"055-444-285"'

    assert_success
    expected=false
    assert_equal "$output" "$expected"
}

@test 'valid strings with symbols included become invalid' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f luhn.jq <<< '"055# 444$ 285"'

    assert_success
    expected=false
    assert_equal "$output" "$expected"
}

@test 'single zero with space is invalid' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f luhn.jq <<< '" 0"'

    assert_success
    expected=false
    assert_equal "$output" "$expected"
}

@test 'more than a single zero is valid' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f luhn.jq <<< '"0000 0"'

    assert_success
    expected=true
    assert_equal "$output" "$expected"
}

@test 'input digit 9 is correctly converted to output digit 9' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f luhn.jq <<< '"091"'

    assert_success
    expected=true
    assert_equal "$output" "$expected"
}

@test 'very long input is valid' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f luhn.jq <<< '"9999999999 9999999999 9999999999 9999999999"'

    assert_success
    expected=true
    assert_equal "$output" "$expected"
}

@test 'valid luhn with an odd number of digits and non zero first digit' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f luhn.jq <<< '"109"'

    assert_success
    expected=true
    assert_equal "$output" "$expected"
}

@test 'using ascii value for non-doubled non-digit isn'\''t allowed' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f luhn.jq <<< '"055b 444 285"'

    assert_success
    expected=false
    assert_equal "$output" "$expected"
}

@test 'using ascii value for doubled non-digit isn'\''t allowed' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f luhn.jq <<< '":9"'

    assert_success
    expected=false
    assert_equal "$output" "$expected"
}

@test 'non-numeric, non-space char in the middle with a sum that'\''s divisible by 10 isn'\''t allowed' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f luhn.jq <<< '"59%59"'

    assert_success
    expected=false
    assert_equal "$output" "$expected"
}
