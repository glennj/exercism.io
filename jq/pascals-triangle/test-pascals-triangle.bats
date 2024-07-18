#!/usr/bin/env bats
# generated on 2024-07-11T22:29:37Z
load bats-extra
load bats-jq

@test 'zero rows' {
    #[[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f pascals-triangle.jq << 'END_INPUT'
        {
          "count": 0
        }
END_INPUT

    assert_success
    expected='[]'
    assert_equal "$output" "$expected"
}

@test 'single row' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f pascals-triangle.jq << 'END_INPUT'
        {
          "count": 1
        }
END_INPUT

    assert_success
    expected='[[1]]'
    assert_equal "$output" "$expected"
}

@test 'two rows' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f pascals-triangle.jq << 'END_INPUT'
        {
          "count": 2
        }
END_INPUT

    assert_success
    expected='[[1],[1,1]]'
    assert_equal "$output" "$expected"
}

@test 'three rows' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f pascals-triangle.jq << 'END_INPUT'
        {
          "count": 3
        }
END_INPUT

    assert_success
    expected='[[1],[1,1],[1,2,1]]'
    assert_equal "$output" "$expected"
}

@test 'four rows' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f pascals-triangle.jq << 'END_INPUT'
        {
          "count": 4
        }
END_INPUT

    assert_success
    expected='[[1],[1,1],[1,2,1],[1,3,3,1]]'
    assert_equal "$output" "$expected"
}

@test 'five rows' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f pascals-triangle.jq << 'END_INPUT'
        {
          "count": 5
        }
END_INPUT

    assert_success
    expected='[[1],[1,1],[1,2,1],[1,3,3,1],[1,4,6,4,1]]'
    assert_equal "$output" "$expected"
}

@test 'six rows' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f pascals-triangle.jq << 'END_INPUT'
        {
          "count": 6
        }
END_INPUT

    assert_success
    expected='[[1],[1,1],[1,2,1],[1,3,3,1],[1,4,6,4,1],[1,5,10,10,5,1]]'
    assert_equal "$output" "$expected"
}

@test 'ten rows' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f pascals-triangle.jq << 'END_INPUT'
        {
          "count": 10
        }
END_INPUT

    assert_success
    expected='[[1],[1,1],[1,2,1],[1,3,3,1],[1,4,6,4,1],[1,5,10,10,5,1],[1,6,15,20,15,6,1],[1,7,21,35,35,21,7,1],[1,8,28,56,70,56,28,8,1],[1,9,36,84,126,126,84,36,9,1]]'
    assert_equal "$output" "$expected"
}
