#!/usr/bin/env bats
# generated on 2026-06-28T19:49:26+00:00
load bats-extra
load bats-jq

@test 'empty' {
    # [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f flatten-array.jq << 'END_INPUT'
        {
          "array": []
        }
END_INPUT

    assert_success
    expected='[]'
    assert_objects_equal "$output" "$expected"
}

@test 'no nesting' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f flatten-array.jq << 'END_INPUT'
        {
          "array": [
            0,
            1,
            2
          ]
        }
END_INPUT

    assert_success
    expected='[0, 1, 2]'
    assert_objects_equal "$output" "$expected"
}

@test 'flattens a nested array' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f flatten-array.jq << 'END_INPUT'
        {
          "array": [
            [
              []
            ]
          ]
        }
END_INPUT

    assert_success
    expected='[]'
    assert_objects_equal "$output" "$expected"
}

@test 'flattens array with just integers present' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f flatten-array.jq << 'END_INPUT'
        {
          "array": [
            1,
            [
              2,
              3,
              4,
              5,
              6,
              7
            ],
            8
          ]
        }
END_INPUT

    assert_success
    expected='[1, 2, 3, 4, 5, 6, 7, 8]'
    assert_objects_equal "$output" "$expected"
}

@test '5 level nesting' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f flatten-array.jq << 'END_INPUT'
        {
          "array": [
            0,
            2,
            [
              [
                2,
                3
              ],
              8,
              100,
              4,
              [
                [
                  [
                    50
                  ]
                ]
              ]
            ],
            -2
          ]
        }
END_INPUT

    assert_success
    expected='[0, 2, 2, 3, 8, 100, 4, 50, -2]'
    assert_objects_equal "$output" "$expected"
}

@test '6 level nesting' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f flatten-array.jq << 'END_INPUT'
        {
          "array": [
            1,
            [
              2,
              [
                [
                  3
                ]
              ],
              [
                4,
                [
                  [
                    5
                  ]
                ]
              ],
              6,
              7
            ],
            8
          ]
        }
END_INPUT

    assert_success
    expected='[1, 2, 3, 4, 5, 6, 7, 8]'
    assert_objects_equal "$output" "$expected"
}

@test 'null values are omitted from the final result' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f flatten-array.jq << 'END_INPUT'
        {
          "array": [
            1,
            2,
            null
          ]
        }
END_INPUT

    assert_success
    expected='[1, 2]'
    assert_objects_equal "$output" "$expected"
}

@test 'consecutive null values at the front of the array are omitted from the final result' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f flatten-array.jq << 'END_INPUT'
        {
          "array": [
            null,
            null,
            3
          ]
        }
END_INPUT

    assert_success
    expected='[3]'
    assert_objects_equal "$output" "$expected"
}

@test 'consecutive null values in the middle of the array are omitted from the final result' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f flatten-array.jq << 'END_INPUT'
        {
          "array": [
            1,
            null,
            null,
            4
          ]
        }
END_INPUT

    assert_success
    expected='[1, 4]'
    assert_objects_equal "$output" "$expected"
}

@test '6 level nested array with null values' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f flatten-array.jq << 'END_INPUT'
        {
          "array": [
            0,
            2,
            [
              [
                2,
                3
              ],
              8,
              [
                [
                  100
                ]
              ],
              null,
              [
                [
                  null
                ]
              ]
            ],
            -2
          ]
        }
END_INPUT

    assert_success
    expected='[0, 2, 2, 3, 8, 100, -2]'
    assert_objects_equal "$output" "$expected"
}

@test 'all values in nested array are null' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f flatten-array.jq << 'END_INPUT'
        {
          "array": [
            null,
            [
              [
                [
                  null
                ]
              ]
            ],
            null,
            null,
            [
              [
                null,
                null
              ],
              null
            ],
            null
          ]
        }
END_INPUT

    assert_success
    expected='[]'
    assert_objects_equal "$output" "$expected"
}
