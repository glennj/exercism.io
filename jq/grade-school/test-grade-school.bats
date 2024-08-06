#!/usr/bin/env bats
# generated on 2024-07-23T00:57:34Z
load bats-extra
load bats-jq

@test 'Roster is empty when no student is added' {
    #[[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f grade-school.jq << 'END_INPUT'
        {
          "property": "roster",
          "input": {
            "students": []
          }
        }
END_INPUT

    assert_success
    expected='[]'
    assert_equal "$output" "$expected"
}

@test 'Student is added to the roster' {
    #[[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f grade-school.jq << 'END_INPUT'
        {
          "property": "roster",
          "input": {
            "students": [
              [
                "Aimee",
                2
              ]
            ]
          }
        }
END_INPUT

    assert_success
    expected='["Aimee"]'
    assert_equal "$output" "$expected"
}

@test 'Multiple students in the same grade are added to the roster' {
    #[[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f grade-school.jq << 'END_INPUT'
        {
          "property": "roster",
          "input": {
            "students": [
              [
                "Blair",
                2
              ],
              [
                "James",
                2
              ],
              [
                "Paul",
                2
              ]
            ]
          }
        }
END_INPUT

    assert_success
    expected='["Blair","James","Paul"]'
    assert_equal "$output" "$expected"
}

@test 'Student not added to same grade in the roster more than once' {
    #[[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f grade-school.jq << 'END_INPUT'
        {
          "property": "roster",
          "input": {
            "students": [
              [
                "Blair",
                2
              ],
              [
                "James",
                2
              ],
              [
                "James",
                2
              ],
              [
                "Paul",
                2
              ]
            ]
          }
        }
END_INPUT

    assert_success
    expected='["Blair","James","Paul"]'
    assert_equal "$output" "$expected"
}

@test 'Students in multiple grades are added to the roster' {
    #[[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f grade-school.jq << 'END_INPUT'
        {
          "property": "roster",
          "input": {
            "students": [
              [
                "Chelsea",
                3
              ],
              [
                "Logan",
                7
              ]
            ]
          }
        }
END_INPUT

    assert_success
    expected='["Chelsea","Logan"]'
    assert_equal "$output" "$expected"
}

@test 'Student not added to multiple grades in the roster' {
    #[[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f grade-school.jq << 'END_INPUT'
        {
          "property": "roster",
          "input": {
            "students": [
              [
                "Blair",
                2
              ],
              [
                "James",
                2
              ],
              [
                "James",
                3
              ],
              [
                "Paul",
                3
              ]
            ]
          }
        }
END_INPUT

    assert_success
    expected='["Blair","James","Paul"]'
    assert_equal "$output" "$expected"
}

@test 'Students are sorted by grades in the roster' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f grade-school.jq << 'END_INPUT'
        {
          "property": "roster",
          "input": {
            "students": [
              [
                "Jim",
                3
              ],
              [
                "Peter",
                2
              ],
              [
                "Anna",
                1
              ]
            ]
          }
        }
END_INPUT

    assert_success
    expected='["Anna","Peter","Jim"]'
    assert_equal "$output" "$expected"
}

@test 'Students are sorted by name in the roster' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f grade-school.jq << 'END_INPUT'
        {
          "property": "roster",
          "input": {
            "students": [
              [
                "Peter",
                2
              ],
              [
                "Zoe",
                2
              ],
              [
                "Alex",
                2
              ]
            ]
          }
        }
END_INPUT

    assert_success
    expected='["Alex","Peter","Zoe"]'
    assert_equal "$output" "$expected"
}

@test 'Students are sorted by grades and then by name in the roster' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f grade-school.jq << 'END_INPUT'
        {
          "property": "roster",
          "input": {
            "students": [
              [
                "Peter",
                2
              ],
              [
                "Anna",
                1
              ],
              [
                "Barb",
                1
              ],
              [
                "Zoe",
                2
              ],
              [
                "Alex",
                2
              ],
              [
                "Jim",
                3
              ],
              [
                "Charlie",
                1
              ]
            ]
          }
        }
END_INPUT

    assert_success
    expected='["Anna","Barb","Charlie","Alex","Peter","Zoe","Jim"]'
    assert_equal "$output" "$expected"
}

@test 'Grade is empty if no students in the roster' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f grade-school.jq << 'END_INPUT'
        {
          "property": "grade",
          "input": {
            "students": [],
            "desiredGrade": 1
          }
        }
END_INPUT

    assert_success
    expected='[]'
    assert_equal "$output" "$expected"
}

@test 'Grade is empty if no students in that grade' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f grade-school.jq << 'END_INPUT'
        {
          "property": "grade",
          "input": {
            "students": [
              [
                "Peter",
                2
              ],
              [
                "Zoe",
                2
              ],
              [
                "Alex",
                2
              ],
              [
                "Jim",
                3
              ]
            ],
            "desiredGrade": 1
          }
        }
END_INPUT

    assert_success
    expected='[]'
    assert_equal "$output" "$expected"
}

@test 'Student not added to same grade more than once' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f grade-school.jq << 'END_INPUT'
        {
          "property": "grade",
          "input": {
            "students": [
              [
                "Blair",
                2
              ],
              [
                "James",
                2
              ],
              [
                "James",
                2
              ],
              [
                "Paul",
                2
              ]
            ],
            "desiredGrade": 2
          }
        }
END_INPUT

    assert_success
    expected='["Blair","James","Paul"]'
    assert_equal "$output" "$expected"
}

@test 'Student not added to multiple grades' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f grade-school.jq << 'END_INPUT'
        {
          "property": "grade",
          "input": {
            "students": [
              [
                "Blair",
                2
              ],
              [
                "James",
                2
              ],
              [
                "James",
                3
              ],
              [
                "Paul",
                3
              ]
            ],
            "desiredGrade": 2
          }
        }
END_INPUT

    assert_success
    expected='["Blair","James"]'
    assert_equal "$output" "$expected"
}

@test 'Student not added to other grade for multiple grades' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f grade-school.jq << 'END_INPUT'
        {
          "property": "grade",
          "input": {
            "students": [
              [
                "Blair",
                2
              ],
              [
                "James",
                2
              ],
              [
                "James",
                3
              ],
              [
                "Paul",
                3
              ]
            ],
            "desiredGrade": 3
          }
        }
END_INPUT

    assert_success
    expected='["Paul"]'
    assert_equal "$output" "$expected"
}

@test 'Students are sorted by name in a grade' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f grade-school.jq << 'END_INPUT'
        {
          "property": "grade",
          "input": {
            "students": [
              [
                "Franklin",
                5
              ],
              [
                "Bradley",
                5
              ],
              [
                "Jeff",
                1
              ]
            ],
            "desiredGrade": 5
          }
        }
END_INPUT

    assert_success
    expected='["Bradley","Franklin"]'
    assert_equal "$output" "$expected"
}
