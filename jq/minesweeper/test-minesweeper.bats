#!/usr/bin/env bats
# generated on 2024-07-11T19:55:31Z
load bats-extra
load bats-jq

@test 'no rows' {
    #[[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -s -R -c -f minesweeper.jq < /dev/null

    assert_success
    expected='[]'
    assert_equal "$output" "$expected"
}

@test 'no columns' {
    #[[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -s -R -c -f minesweeper.jq <<< ""

    assert_success
    expected='[""]'
    assert_equal "$output" "$expected"
}

@test 'no mines' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -s -R -c -f minesweeper.jq << 'END_INPUT'
...
...
...
END_INPUT

    assert_success
    expected='["...","...","..."]'
    assert_equal "$output" "$expected"
}

@test 'minefield with only mines' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -s -R -c -f minesweeper.jq << 'END_INPUT'
***
***
***
END_INPUT

    assert_success
    expected='["***","***","***"]'
    assert_equal "$output" "$expected"
}

@test 'mine surrounded by spaces' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -s -R -c -f minesweeper.jq << 'END_INPUT'
...
.*.
...
END_INPUT

    assert_success
    expected='["111","1*1","111"]'
    assert_equal "$output" "$expected"
}

@test 'space surrounded by mines' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -s -R -c -f minesweeper.jq << 'END_INPUT'
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

    run jq -s -R -c -f minesweeper.jq << 'END_INPUT'
.*.*.
END_INPUT

    assert_success
    expected='["1*2*1"]'
    assert_equal "$output" "$expected"
}

@test 'horizontal line, mines at edges' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -s -R -c -f minesweeper.jq << 'END_INPUT'
*...*
END_INPUT

    assert_success
    expected='["*1.1*"]'
    assert_equal "$output" "$expected"
}

@test 'vertical line' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -s -R -c -f minesweeper.jq << 'END_INPUT'
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

@test 'vertical line, mines at edges' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -s -R -c -f minesweeper.jq << 'END_INPUT'
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

    run jq -s -R -c -f minesweeper.jq << 'END_INPUT'
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

@test 'large minefield' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -s -R -c -f minesweeper.jq << 'END_INPUT'
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
