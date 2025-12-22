#!/usr/bin/env bats
# generated on 2023-11-07T18:49:22Z
load bats-extra
load bats-jq

assert_objects_equal() {
    local result=$(
        jq -n --argjson actual "$1" \
              --argjson expected "$2" \
            '$actual == $expected'
    )
    [[ $result == "true" ]]
}

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
    assert_objects_equal "$output" "$expected"
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
    assert_objects_equal "$output" "$expected"
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
    assert_objects_equal "$output" "$expected"
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

@test "A degenerate binary tree" {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f satellite.jq << 'END_INPUT'
      {
        "preorder": ["a", "b", "c", "d"],
        "inorder": ["d", "c", "b", "a"]
      }
END_INPUT

    assert_success
    expected='{"v":"a","l":{"v":"b","l":{"v":"c","l":{"v":"d","l":{},"r":{}},"r":{}},"r":{}},"r":{}}'
    assert_objects_equal "$output" "$expected"
}

@test "Another degenerate binary tree" {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f satellite.jq << 'END_INPUT'
      {
        "preorder": ["a", "b", "c", "d"],
        "inorder": ["a", "b", "c", "d"]
      }
END_INPUT

    assert_success
    expected='{"v":"a","l":{},"r":{"v":"b","l":{},"r":{"v":"c","l":{},"r":{"v":"d","l":{},"r":{}}}}}'
    assert_objects_equal "$output" "$expected"
}

@test "Tree with many more items" {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f satellite.jq << 'END_INPUT'
      {
        "preorder": ["a", "b", "d", "g", "h", "c", "e", "f", "i"],
        "inorder": ["g", "d", "h", "b", "a", "e", "c", "i", "f"]
      }
END_INPUT

    assert_success
    expected='{"v":"a","l":{"v":"b","l":{"v":"d","l":{"v":"g","l":{},"r":{}},"r":{"v":"h","l":{},"r":{}}},"r":{}},"r":{"v":"c","l":{"v":"e","l":{},"r":{}},"r":{"v":"f","l":{"v":"i","l":{},"r":{}},"r":{}}}}'
    assert_objects_equal "$output" "$expected"
}
