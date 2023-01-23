#!/usr/bin/env bats
# generated on 2022-11-02T20:59:46Z
load bats-extra

@test 'Empty tree' {
    #[[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f satellite.jq << 'END_INPUT'
        {
          "preorder": [],
          "inorder": []
        }
END_INPUT

    assert_success
    expected='{}'
    assert_equal "$output" "$expected"
}

@test 'Tree with one item' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f satellite.jq << 'END_INPUT'
        {
          "preorder": [
            "a"
          ],
          "inorder": [
            "a"
          ]
        }
END_INPUT

    assert_success
    expected='{"v":"a","l":{},"r":{}}'
    assert_equal "$output" "$expected"
}

@test 'Tree with many items' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f satellite.jq << 'END_INPUT'
        {
          "preorder": [
            "a",
            "i",
            "x",
            "f",
            "r"
          ],
          "inorder": [
            "i",
            "a",
            "f",
            "x",
            "r"
          ]
        }
END_INPUT

    assert_success
    expected='{"v":"a","l":{"v":"i","l":{},"r":{}},"r":{"v":"x","l":{"v":"f","l":{},"r":{}},"r":{"v":"r","l":{},"r":{}}}}'
    assert_equal "$output" "$expected"
}

@test 'Reject traversals of different length' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f satellite.jq << 'END_INPUT'
        {
          "preorder": [
            "a",
            "b"
          ],
          "inorder": [
            "b",
            "a",
            "r"
          ]
        }
END_INPUT

    assert_failure
    expected='traversals must have the same length'
    assert_equal "$output" "$expected"
}

@test 'Reject inconsistent traversals of same length' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f satellite.jq << 'END_INPUT'
        {
          "preorder": [
            "x",
            "y",
            "z"
          ],
          "inorder": [
            "a",
            "b",
            "c"
          ]
        }
END_INPUT

    assert_failure
    expected='traversals must have the same elements'
    assert_equal "$output" "$expected"
}

@test 'Reject traversals with repeated items' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f satellite.jq << 'END_INPUT'
        {
          "preorder": [
            "a",
            "b",
            "a"
          ],
          "inorder": [
            "b",
            "a",
            "a"
          ]
        }
END_INPUT

    assert_failure
    expected='traversals must contain unique items'
    assert_equal "$output" "$expected"
}

