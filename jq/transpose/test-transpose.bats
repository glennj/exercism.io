#!/usr/bin/env bats
# generated on 2022-11-02T20:59:51Z
load bats-extra

@test 'empty string' {
    #[[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f transpose.jq << 'END_INPUT'
        {
          "lines": []
        }
END_INPUT

    assert_success
    expected='[]'
    assert_equal "$output" "$expected"
}

@test 'two characters in a row' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f transpose.jq << 'END_INPUT'
        {
          "lines": [
            "A1"
          ]
        }
END_INPUT

    assert_success
    expected='["A","1"]'
    assert_equal "$output" "$expected"
}

@test 'two characters in a column' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f transpose.jq << 'END_INPUT'
        {
          "lines": [
            "A",
            "1"
          ]
        }
END_INPUT

    assert_success
    expected='["A1"]'
    assert_equal "$output" "$expected"
}

@test 'simple' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f transpose.jq << 'END_INPUT'
        {
          "lines": [
            "ABC",
            "123"
          ]
        }
END_INPUT

    assert_success
    expected='["A1","B2","C3"]'
    assert_equal "$output" "$expected"
}

@test 'single line' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f transpose.jq << 'END_INPUT'
        {
          "lines": [
            "Single line."
          ]
        }
END_INPUT

    assert_success
    expected='["S","i","n","g","l","e"," ","l","i","n","e","."]'
    assert_equal "$output" "$expected"
}

@test 'first line longer than second line' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f transpose.jq << 'END_INPUT'
        {
          "lines": [
            "The fourth line.",
            "The fifth line."
          ]
        }
END_INPUT

    assert_success
    expected='["TT","hh","ee","  ","ff","oi","uf","rt","th","h "," l","li","in","ne","e.","."]'
    assert_equal "$output" "$expected"
}

@test 'second line longer than first line' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f transpose.jq << 'END_INPUT'
        {
          "lines": [
            "The first line.",
            "The second line."
          ]
        }
END_INPUT

    assert_success
    expected='["TT","hh","ee","  ","fs","ie","rc","so","tn"," d","l ","il","ni","en",".e"," ."]'
    assert_equal "$output" "$expected"
}

@test 'mixed line length' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f transpose.jq << 'END_INPUT'
        {
          "lines": [
            "The longest line.",
            "A long line.",
            "A longer line.",
            "A line."
          ]
        }
END_INPUT

    assert_success
    expected='["TAAA","h   ","elll"," ooi","lnnn","ogge","n e.","glr","ei ","snl","tei"," .n","l e","i .","n","e","."]'
    assert_equal "$output" "$expected"
}

@test 'square' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f transpose.jq << 'END_INPUT'
        {
          "lines": [
            "HEART",
            "EMBER",
            "ABUSE",
            "RESIN",
            "TREND"
          ]
        }
END_INPUT

    assert_success
    expected='["HEART","EMBER","ABUSE","RESIN","TREND"]'
    assert_equal "$output" "$expected"
}

@test 'rectangle' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f transpose.jq << 'END_INPUT'
        {
          "lines": [
            "FRACTURE",
            "OUTLINED",
            "BLOOMING",
            "SEPTETTE"
          ]
        }
END_INPUT

    assert_success
    expected='["FOBS","RULE","ATOP","CLOT","TIME","UNIT","RENT","EDGE"]'
    assert_equal "$output" "$expected"
}

@test 'triangle' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f transpose.jq << 'END_INPUT'
        {
          "lines": [
            "T",
            "EE",
            "AAA",
            "SSSS",
            "EEEEE",
            "RRRRRR"
          ]
        }
END_INPUT

    assert_success
    expected='["TEASER"," EASER","  ASER","   SER","    ER","     R"]'
    assert_equal "$output" "$expected"
}

@test 'jagged triangle' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f transpose.jq << 'END_INPUT'
        {
          "lines": [
            "11",
            "2",
            "3333",
            "444",
            "555555",
            "66666"
          ]
        }
END_INPUT

    assert_success
    expected='["123456","1 3456","  3456","  3 56","    56","    5"]'
    assert_equal "$output" "$expected"
}

