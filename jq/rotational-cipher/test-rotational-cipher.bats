#!/usr/bin/env bats
# generated on 
load bats-extra

@test 'rotate a by 0, same output as input' {
    #[[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f rotational-cipher.jq << 'END_INPUT'
        {
          "text": "a",
          "shiftKey": 0
        }
END_INPUT

    assert_success
    expected='a'
    assert_equal "$output" "$expected"
}

@test 'rotate a by 1' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f rotational-cipher.jq << 'END_INPUT'
        {
          "text": "a",
          "shiftKey": 1
        }
END_INPUT

    assert_success
    expected='b'
    assert_equal "$output" "$expected"
}

@test 'rotate a by 26, same output as input' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f rotational-cipher.jq << 'END_INPUT'
        {
          "text": "a",
          "shiftKey": 26
        }
END_INPUT

    assert_success
    expected='a'
    assert_equal "$output" "$expected"
}

@test 'rotate m by 13' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f rotational-cipher.jq << 'END_INPUT'
        {
          "text": "m",
          "shiftKey": 13
        }
END_INPUT

    assert_success
    expected='z'
    assert_equal "$output" "$expected"
}

@test 'rotate n by 13 with wrap around alphabet' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f rotational-cipher.jq << 'END_INPUT'
        {
          "text": "n",
          "shiftKey": 13
        }
END_INPUT

    assert_success
    expected='a'
    assert_equal "$output" "$expected"
}

@test 'rotate capital letters' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f rotational-cipher.jq << 'END_INPUT'
        {
          "text": "OMG",
          "shiftKey": 5
        }
END_INPUT

    assert_success
    expected='TRL'
    assert_equal "$output" "$expected"
}

@test 'rotate spaces' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f rotational-cipher.jq << 'END_INPUT'
        {
          "text": "O M G",
          "shiftKey": 5
        }
END_INPUT

    assert_success
    expected='T R L'
    assert_equal "$output" "$expected"
}

@test 'rotate numbers' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f rotational-cipher.jq << 'END_INPUT'
        {
          "text": "Testing 1 2 3 testing",
          "shiftKey": 4
        }
END_INPUT

    assert_success
    expected='Xiwxmrk 1 2 3 xiwxmrk'
    assert_equal "$output" "$expected"
}

@test 'rotate punctuation' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f rotational-cipher.jq << 'END_INPUT'
        {
          "text": "Let's eat, Grandma!",
          "shiftKey": 21
        }
END_INPUT

    assert_success
    expected='Gzo'\''n zvo, Bmviyhv!'
    assert_equal "$output" "$expected"
}

@test 'rotate all letters' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f rotational-cipher.jq << 'END_INPUT'
        {
          "text": "The quick brown fox jumps over the lazy dog.",
          "shiftKey": 13
        }
END_INPUT

    assert_success
    expected='Gur dhvpx oebja sbk whzcf bire gur ynml qbt.'
    assert_equal "$output" "$expected"
}
