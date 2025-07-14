#!/usr/bin/env bats
load bats-extra
load bats-jq

@test 'no rows' {
    #[[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -s -R -c -f flower-field.jq < /dev/null

    assert_success
    expected='[]'
    assert_equal "$output" "$expected"
}

@test 'no columns' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -s -R -c -f flower-field.jq <<< ""

    assert_success
    expected='[""]'
    assert_equal "$output" "$expected"
}

@test 'no flowers' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -s -R -c -f flower-field.jq << 'END_INPUT'
...
...
...
END_INPUT

    assert_success
    expected='["...","...","..."]'
    assert_equal "$output" "$expected"
}

@test 'garden full of flowers' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -s -R -c -f flower-field.jq << 'END_INPUT'
***
***
***
END_INPUT

    assert_success
    expected='["***","***","***"]'
    assert_equal "$output" "$expected"
}

@test 'flower surrounded by spaces' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -s -R -c -f flower-field.jq << 'END_INPUT'
...
.*.
...
END_INPUT

    assert_success
    expected='["111","1*1","111"]'
    assert_equal "$output" "$expected"
}

@test 'space surrounded by flowers' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -s -R -c -f flower-field.jq << 'END_INPUT'
***
*.*
***
END_INPUT

    assert_success
    expected='["***","*8*","***"]'
    assert_equal "$output" "$expected"
}

@test 'horizontal line' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -s -R -c -f flower-field.jq << 'END_INPUT'
.*.*.
END_INPUT

    assert_success
    expected='["1*2*1"]'
    assert_equal "$output" "$expected"
}

@test 'horizontal line, flowers at edges' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -s -R -c -f flower-field.jq << 'END_INPUT'
*...*
END_INPUT

    assert_success
    expected='["*1.1*"]'
    assert_equal "$output" "$expected"
}

@test 'vertical line' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -s -R -c -f flower-field.jq << 'END_INPUT'
.
*
.
*
.
END_INPUT

    assert_success
    expected='["1","*","2","*","1"]'
    assert_equal "$output" "$expected"
}

@test 'vertical line, flowers at edges' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -s -R -c -f flower-field.jq << 'END_INPUT'
*
.
.
.
*
END_INPUT

    assert_success
    expected='["*","1",".","1","*"]'
    assert_equal "$output" "$expected"
}

@test 'cross' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -s -R -c -f flower-field.jq << 'END_INPUT'
..*..
..*..
*****
..*..
..*..
END_INPUT

    assert_success
    expected='[".2*2.","25*52","*****","25*52",".2*2."]'
    assert_equal "$output" "$expected"
}

@test 'large garden' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -s -R -c -f flower-field.jq << 'END_INPUT'
.*..*.
..*...
....*.
...*.*
.*..*.
......
END_INPUT

    assert_success
    expected='["1*22*1","12*322",".123*2","112*4*","1*22*2","111111"]'
    assert_equal "$output" "$expected"
}
