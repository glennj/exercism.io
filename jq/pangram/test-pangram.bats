#!/usr/bin/env bats
# generated on 
load bats-extra
load bats-jq

@test 'empty sentence' {
    #[[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f pangram.jq << 'END_INPUT'
        {
          "sentence": ""
        }
END_INPUT

    assert_success
    expected=false
    assert_equal "$output" "$expected"
}

@test 'perfect lower case' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f pangram.jq << 'END_INPUT'
        {
          "sentence": "abcdefghijklmnopqrstuvwxyz"
        }
END_INPUT

    assert_success
    expected=true
    assert_equal "$output" "$expected"
}

@test 'only lower case' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f pangram.jq << 'END_INPUT'
        {
          "sentence": "the quick brown fox jumps over the lazy dog"
        }
END_INPUT

    assert_success
    expected=true
    assert_equal "$output" "$expected"
}

@test 'missing the letter "x"' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f pangram.jq << 'END_INPUT'
        {
          "sentence": "a quick movement of the enemy will jeopardize five gunboats"
        }
END_INPUT

    assert_success
    expected=false
    assert_equal "$output" "$expected"
}

@test 'missing the letter "h"' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f pangram.jq << 'END_INPUT'
        {
          "sentence": "five boxing wizards jump quickly at it"
        }
END_INPUT

    assert_success
    expected=false
    assert_equal "$output" "$expected"
}

@test 'with underscores' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f pangram.jq << 'END_INPUT'
        {
          "sentence": "the_quick_brown_fox_jumps_over_the_lazy_dog"
        }
END_INPUT

    assert_success
    expected=true
    assert_equal "$output" "$expected"
}

@test 'with numbers' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f pangram.jq << 'END_INPUT'
        {
          "sentence": "the 1 quick brown fox jumps over the 2 lazy dogs"
        }
END_INPUT

    assert_success
    expected=true
    assert_equal "$output" "$expected"
}

@test 'missing letters replaced by numbers' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f pangram.jq << 'END_INPUT'
        {
          "sentence": "7h3 qu1ck brown fox jumps ov3r 7h3 lazy dog"
        }
END_INPUT

    assert_success
    expected=false
    assert_equal "$output" "$expected"
}

@test 'mixed case and punctuation' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f pangram.jq << 'END_INPUT'
        {
          "sentence": "\"Five quacking Zephyrs jolt my wax bed.\""
        }
END_INPUT

    assert_success
    expected=true
    assert_equal "$output" "$expected"
}

@test 'a-m and A-M are 26 different characters but not a pangram' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f pangram.jq << 'END_INPUT'
        {
          "sentence": "abcdefghijklm ABCDEFGHIJKLM"
        }
END_INPUT

    assert_success
    expected=false
    assert_equal "$output" "$expected"
}
