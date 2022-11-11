#!/usr/bin/env bats
# generated on 2022-11-02T20:59:14Z
load bats-extra

@test 'empty string' {
    #[[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f isogram.jq << 'END_INPUT'
        {
          "phrase": ""
        }
END_INPUT

    assert_success
    expected=true
    assert_equal "$output" "$expected"
}

@test 'isogram with only lower case characters' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f isogram.jq << 'END_INPUT'
        {
          "phrase": "isogram"
        }
END_INPUT

    assert_success
    expected=true
    assert_equal "$output" "$expected"
}

@test 'word with one duplicated character' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f isogram.jq << 'END_INPUT'
        {
          "phrase": "eleven"
        }
END_INPUT

    assert_success
    expected=false
    assert_equal "$output" "$expected"
}

@test 'word with one duplicated character from the end of the alphabet' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f isogram.jq << 'END_INPUT'
        {
          "phrase": "zzyzx"
        }
END_INPUT

    assert_success
    expected=false
    assert_equal "$output" "$expected"
}

@test 'longest reported english isogram' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f isogram.jq << 'END_INPUT'
        {
          "phrase": "subdermatoglyphic"
        }
END_INPUT

    assert_success
    expected=true
    assert_equal "$output" "$expected"
}

@test 'word with duplicated character in mixed case' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f isogram.jq << 'END_INPUT'
        {
          "phrase": "Alphabet"
        }
END_INPUT

    assert_success
    expected=false
    assert_equal "$output" "$expected"
}

@test 'word with duplicated character in mixed case, lowercase first' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f isogram.jq << 'END_INPUT'
        {
          "phrase": "alphAbet"
        }
END_INPUT

    assert_success
    expected=false
    assert_equal "$output" "$expected"
}

@test 'hypothetical isogrammic word with hyphen' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f isogram.jq << 'END_INPUT'
        {
          "phrase": "thumbscrew-japingly"
        }
END_INPUT

    assert_success
    expected=true
    assert_equal "$output" "$expected"
}

@test 'hypothetical word with duplicated character following hyphen' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f isogram.jq << 'END_INPUT'
        {
          "phrase": "thumbscrew-jappingly"
        }
END_INPUT

    assert_success
    expected=false
    assert_equal "$output" "$expected"
}

@test 'isogram with duplicated hyphen' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f isogram.jq << 'END_INPUT'
        {
          "phrase": "six-year-old"
        }
END_INPUT

    assert_success
    expected=true
    assert_equal "$output" "$expected"
}

@test 'made-up name that is an isogram' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f isogram.jq << 'END_INPUT'
        {
          "phrase": "Emily Jung Schwartzkopf"
        }
END_INPUT

    assert_success
    expected=true
    assert_equal "$output" "$expected"
}

@test 'duplicated character in the middle' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f isogram.jq << 'END_INPUT'
        {
          "phrase": "accentor"
        }
END_INPUT

    assert_success
    expected=false
    assert_equal "$output" "$expected"
}

@test 'same first and last characters' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f isogram.jq << 'END_INPUT'
        {
          "phrase": "angola"
        }
END_INPUT

    assert_success
    expected=false
    assert_equal "$output" "$expected"
}

@test 'word with duplicated character and with two hyphens' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f isogram.jq << 'END_INPUT'
        {
          "phrase": "up-to-date"
        }
END_INPUT

    assert_success
    expected=false
    assert_equal "$output" "$expected"
}

