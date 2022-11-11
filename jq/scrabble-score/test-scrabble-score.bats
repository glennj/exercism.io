#!/usr/bin/env bats
# generated on 2022-11-02T20:59:47Z
load bats-extra

@test 'lowercase letter' {
    #[[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f scrabble-score.jq << 'END_INPUT'
        {
          "word": "a"
        }
END_INPUT

    assert_success
    expected=1
    assert_equal "$output" "$expected"
}

@test 'uppercase letter' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f scrabble-score.jq << 'END_INPUT'
        {
          "word": "A"
        }
END_INPUT

    assert_success
    expected=1
    assert_equal "$output" "$expected"
}

@test 'valuable letter' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f scrabble-score.jq << 'END_INPUT'
        {
          "word": "f"
        }
END_INPUT

    assert_success
    expected=4
    assert_equal "$output" "$expected"
}

@test 'short word' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f scrabble-score.jq << 'END_INPUT'
        {
          "word": "at"
        }
END_INPUT

    assert_success
    expected=2
    assert_equal "$output" "$expected"
}

@test 'short, valuable word' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f scrabble-score.jq << 'END_INPUT'
        {
          "word": "zoo"
        }
END_INPUT

    assert_success
    expected=12
    assert_equal "$output" "$expected"
}

@test 'medium word' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f scrabble-score.jq << 'END_INPUT'
        {
          "word": "street"
        }
END_INPUT

    assert_success
    expected=6
    assert_equal "$output" "$expected"
}

@test 'medium, valuable word' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f scrabble-score.jq << 'END_INPUT'
        {
          "word": "quirky"
        }
END_INPUT

    assert_success
    expected=22
    assert_equal "$output" "$expected"
}

@test 'long, mixed-case word' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f scrabble-score.jq << 'END_INPUT'
        {
          "word": "OxyphenButazone"
        }
END_INPUT

    assert_success
    expected=41
    assert_equal "$output" "$expected"
}

@test 'english-like word' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f scrabble-score.jq << 'END_INPUT'
        {
          "word": "pinata"
        }
END_INPUT

    assert_success
    expected=8
    assert_equal "$output" "$expected"
}

@test 'empty input' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f scrabble-score.jq << 'END_INPUT'
        {
          "word": ""
        }
END_INPUT

    assert_success
    expected=0
    assert_equal "$output" "$expected"
}

@test 'entire alphabet available' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f scrabble-score.jq << 'END_INPUT'
        {
          "word": "abcdefghijklmnopqrstuvwxyz"
        }
END_INPUT

    assert_success
    expected=87
    assert_equal "$output" "$expected"
}

