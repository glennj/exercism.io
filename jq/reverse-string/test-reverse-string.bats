#!/usr/bin/env bats
# generated on 2022-11-02T20:59:42Z
load bats-extra
load bats-jq

@test 'an empty string' {
    #[[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f reverse-string.jq << 'END_INPUT'
        {
          "value": ""
        }
END_INPUT

    assert_success
    expected=''
    assert_equal "$output" "$expected"
}

@test 'a word' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f reverse-string.jq << 'END_INPUT'
        {
          "value": "robot"
        }
END_INPUT

    assert_success
    expected='tobor'
    assert_equal "$output" "$expected"
}

@test 'a capitalized word' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f reverse-string.jq << 'END_INPUT'
        {
          "value": "Ramen"
        }
END_INPUT

    assert_success
    expected='nemaR'
    assert_equal "$output" "$expected"
}

@test 'a sentence with punctuation' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f reverse-string.jq << 'END_INPUT'
        {
          "value": "I'm hungry!"
        }
END_INPUT

    assert_success
    expected='!yrgnuh m'\''I'
    assert_equal "$output" "$expected"
}

@test 'a palindrome' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f reverse-string.jq << 'END_INPUT'
        {
          "value": "racecar"
        }
END_INPUT

    assert_success
    expected='racecar'
    assert_equal "$output" "$expected"
}

@test 'an even-sized word' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f reverse-string.jq << 'END_INPUT'
        {
          "value": "drawer"
        }
END_INPUT

    assert_success
    expected='reward'
    assert_equal "$output" "$expected"
}

@test 'wide characters' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f reverse-string.jq << 'END_INPUT'
        {
          "value": "子猫"
        }
END_INPUT

    assert_success
    expected='猫子'
    assert_equal "$output" "$expected"
}

