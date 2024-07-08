#!/usr/bin/env bats
# generated on 2024-06-28T22:33:39Z
load bats-extra
load bats-jq

@test 'empty spiral' {
    #[[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f spiral-matrix.jq << 'END_INPUT'
        {
          "size": 0
        }
END_INPUT

    assert_success
    expected='[]'
    assert_equal "$output" "$expected"
}

@test 'trivial spiral' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f spiral-matrix.jq << 'END_INPUT'
        {
          "size": 1
        }
END_INPUT

    assert_success
    expected='[[1]]'
    assert_equal "$output" "$expected"
}

@test 'spiral of size 2' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f spiral-matrix.jq << 'END_INPUT'
        {
          "size": 2
        }
END_INPUT
    expected=$(jq -c . <<'END_EXPECTED'
        [
            [1, 2],
            [4, 3]
        ]
END_EXPECTED
)

    assert_success
    assert_equal "$output" "$expected"
}

@test 'spiral of size 3' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f spiral-matrix.jq << 'END_INPUT'
        {
          "size": 3
        }
END_INPUT
    expected=$(jq -c . <<'END_EXPECTED'
        [
            [1, 2, 3],
            [8, 9, 4],
            [7, 6, 5]
        ]
END_EXPECTED
)

    assert_success
    assert_equal "$output" "$expected"
}

@test 'spiral of size 4' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f spiral-matrix.jq << 'END_INPUT'
        {
          "size": 4
        }
END_INPUT
    expected=$(jq -c . <<'END_EXPECTED'
        [
            [1, 2, 3, 4],
            [12, 13, 14, 5],
            [11, 16, 15, 6],
            [10, 9, 8, 7]
        ]
END_EXPECTED
)

    assert_success
    assert_equal "$output" "$expected"
}

@test 'spiral of size 5' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f spiral-matrix.jq << 'END_INPUT'
        {
          "size": 5
        }
END_INPUT
    expected=$(jq -c . <<'END_EXPECTED'
        [
            [1, 2, 3, 4, 5],
            [16, 17, 18, 19, 6],
            [15, 24, 25, 20, 7],
            [14, 23, 22, 21, 8],
            [13, 12, 11, 10, 9]
        ]
END_EXPECTED
)

    assert_success
    assert_equal "$output" "$expected"
}
