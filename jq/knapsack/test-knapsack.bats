#!/usr/bin/env bats
# generated on 2022-11-02T20:59:16Z
load bats-extra
load bats-jq

@test 'no items' {
    #[[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f knapsack.jq << 'END_INPUT'
        {
          "maximumWeight": 100,
          "items": {}
        }
END_INPUT

    assert_success
    expected=0
    assert_equal "$output" "$expected"
}

@test 'one item, too heavy' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f knapsack.jq << 'END_INPUT'
        {
          "maximumWeight": 10,
          "items": [
            {
              "weight": 100,
              "value": 1
            }
          ]
        }
END_INPUT

    assert_success
    expected=0
    assert_equal "$output" "$expected"
}

@test 'five items (cannot be greedy by weight)' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f knapsack.jq << 'END_INPUT'
        {
          "maximumWeight": 10,
          "items": [
            {
              "weight": 2,
              "value": 5
            },
            {
              "weight": 2,
              "value": 5
            },
            {
              "weight": 2,
              "value": 5
            },
            {
              "weight": 2,
              "value": 5
            },
            {
              "weight": 10,
              "value": 21
            }
          ]
        }
END_INPUT

    assert_success
    expected=21
    assert_equal "$output" "$expected"
}

@test 'five items (cannot be greedy by value)' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f knapsack.jq << 'END_INPUT'
        {
          "maximumWeight": 10,
          "items": [
            {
              "weight": 2,
              "value": 20
            },
            {
              "weight": 2,
              "value": 20
            },
            {
              "weight": 2,
              "value": 20
            },
            {
              "weight": 2,
              "value": 20
            },
            {
              "weight": 10,
              "value": 50
            }
          ]
        }
END_INPUT

    assert_success
    expected=80
    assert_equal "$output" "$expected"
}

@test 'example knapsack' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f knapsack.jq << 'END_INPUT'
        {
          "maximumWeight": 10,
          "items": [
            {
              "weight": 5,
              "value": 10
            },
            {
              "weight": 4,
              "value": 40
            },
            {
              "weight": 6,
              "value": 30
            },
            {
              "weight": 4,
              "value": 50
            }
          ]
        }
END_INPUT

    assert_success
    expected=90
    assert_equal "$output" "$expected"
}

@test '8 items' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f knapsack.jq << 'END_INPUT'
        {
          "maximumWeight": 104,
          "items": [
            {
              "weight": 25,
              "value": 350
            },
            {
              "weight": 35,
              "value": 400
            },
            {
              "weight": 45,
              "value": 450
            },
            {
              "weight": 5,
              "value": 20
            },
            {
              "weight": 25,
              "value": 70
            },
            {
              "weight": 3,
              "value": 8
            },
            {
              "weight": 2,
              "value": 5
            },
            {
              "weight": 2,
              "value": 5
            }
          ]
        }
END_INPUT

    assert_success
    expected=900
    assert_equal "$output" "$expected"
}

@test '15 items' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f knapsack.jq << 'END_INPUT'
        {
          "maximumWeight": 750,
          "items": [
            {
              "weight": 70,
              "value": 135
            },
            {
              "weight": 73,
              "value": 139
            },
            {
              "weight": 77,
              "value": 149
            },
            {
              "weight": 80,
              "value": 150
            },
            {
              "weight": 82,
              "value": 156
            },
            {
              "weight": 87,
              "value": 163
            },
            {
              "weight": 90,
              "value": 173
            },
            {
              "weight": 94,
              "value": 184
            },
            {
              "weight": 98,
              "value": 192
            },
            {
              "weight": 106,
              "value": 201
            },
            {
              "weight": 110,
              "value": 210
            },
            {
              "weight": 113,
              "value": 214
            },
            {
              "weight": 115,
              "value": 221
            },
            {
              "weight": 118,
              "value": 229
            },
            {
              "weight": 120,
              "value": 240
            }
          ]
        }
END_INPUT

    assert_success
    expected=1458
    assert_equal "$output" "$expected"
}

