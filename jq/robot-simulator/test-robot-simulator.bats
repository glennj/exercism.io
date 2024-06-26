#!/usr/bin/env bats
# generated on 
load bats-extra
load bats-jq

assert_objects_equal() {
    local result=$(
        jq -n --argjson actual "$1" \
              --argjson expected "$2" \
            '$actual == $expected'
    )
    if [[ $result != "true" ]]; then
        echo "expected: $2" >&2
        echo "actual: $1" >&2
        return 1
    fi
}

@test 'Create robot:at origin facing north' {
    #[[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f robot-simulator.jq << 'END_INPUT'
      {
        "robot": {
          "position": {
            "x": 0,
            "y": 0
          },
          "direction": "north"
        }
      }
END_INPUT

    assert_success
    expected='{"position":{"x":0,"y":0},"direction":"north"}'
    assert_objects_equal "$output" "$expected"
}

@test 'Create robot:at negative position facing south' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f robot-simulator.jq << 'END_INPUT'
      {
        "robot": {
          "position": {
            "x": -1,
            "y": -1
          },
          "direction": "south"
        }
      }
END_INPUT

    assert_success
    expected='{"position":{"x":-1,"y":-1},"direction":"south"}'
    assert_objects_equal "$output" "$expected"
}

@test 'Rotating clockwise:changes north to east' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f robot-simulator.jq << 'END_INPUT'
      {
        "robot": {
          "position": {
            "x": 0,
            "y": 0
          },
          "direction": "north"
        },
        "instructions": "R"
      }
END_INPUT

    assert_success
    expected='{"position":{"x":0,"y":0},"direction":"east"}'
    assert_objects_equal "$output" "$expected"
}

@test 'Rotating clockwise:changes east to south' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f robot-simulator.jq << 'END_INPUT'
      {
        "robot": {
          "position": {
            "x": 0,
            "y": 0
          },
          "direction": "east"
        },
        "instructions": "R"
      }
END_INPUT

    assert_success
    expected='{"position":{"x":0,"y":0},"direction":"south"}'
    assert_objects_equal "$output" "$expected"
}

@test 'Rotating clockwise:changes south to west' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f robot-simulator.jq << 'END_INPUT'
      {
        "robot": {
          "position": {
            "x": 0,
            "y": 0
          },
          "direction": "south"
        },
        "instructions": "R"
      }
END_INPUT

    assert_success
    expected='{"position":{"x":0,"y":0},"direction":"west"}'
    assert_objects_equal "$output" "$expected"
}

@test 'Rotating clockwise:changes west to north' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f robot-simulator.jq << 'END_INPUT'
      {
        "robot": {
          "position": {
            "x": 0,
            "y": 0
          },
          "direction": "west"
        },
        "instructions": "R"
      }
END_INPUT

    assert_success
    expected='{"position":{"x":0,"y":0},"direction":"north"}'
    assert_objects_equal "$output" "$expected"
}

@test 'Rotating counter-clockwise:changes north to west' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f robot-simulator.jq << 'END_INPUT'
      {
        "robot": {
          "position": {
            "x": 0,
            "y": 0
          },
          "direction": "north"
        },
        "instructions": "L"
      }
END_INPUT

    assert_success
    expected='{"position":{"x":0,"y":0},"direction":"west"}'
    assert_objects_equal "$output" "$expected"
}

@test 'Rotating counter-clockwise:changes west to south' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f robot-simulator.jq << 'END_INPUT'
      {
        "robot": {
          "position": {
            "x": 0,
            "y": 0
          },
          "direction": "west"
        },
        "instructions": "L"
      }
END_INPUT

    assert_success
    expected='{"position":{"x":0,"y":0},"direction":"south"}'
    assert_objects_equal "$output" "$expected"
}

@test 'Rotating counter-clockwise:changes south to east' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f robot-simulator.jq << 'END_INPUT'
      {
        "robot": {
          "position": {
            "x": 0,
            "y": 0
          },
          "direction": "south"
        },
        "instructions": "L"
      }
END_INPUT

    assert_success
    expected='{"position":{"x":0,"y":0},"direction":"east"}'
    assert_objects_equal "$output" "$expected"
}

@test 'Rotating counter-clockwise:changes east to north' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f robot-simulator.jq << 'END_INPUT'
      {
        "robot": {
          "position": {
            "x": 0,
            "y": 0
          },
          "direction": "east"
        },
        "instructions": "L"
      }
END_INPUT

    assert_success
    expected='{"position":{"x":0,"y":0},"direction":"north"}'
    assert_objects_equal "$output" "$expected"
}

@test 'Moving forward one:facing north increments Y' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f robot-simulator.jq << 'END_INPUT'
      {
        "robot": {
          "position": {
            "x": 0,
            "y": 0
          },
          "direction": "north"
        },
        "instructions": "A"
      }
END_INPUT

    assert_success
    expected='{"position":{"x":0,"y":1},"direction":"north"}'
    assert_objects_equal "$output" "$expected"
}

@test 'Moving forward one:facing south decrements Y' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f robot-simulator.jq << 'END_INPUT'
      {
        "robot": {
          "position": {
            "x": 0,
            "y": 0
          },
          "direction": "south"
        },
        "instructions": "A"
      }
END_INPUT

    assert_success
    expected='{"position":{"x":0,"y":-1},"direction":"south"}'
    assert_objects_equal "$output" "$expected"
}

@test 'Moving forward one:facing east increments X' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f robot-simulator.jq << 'END_INPUT'
      {
        "robot": {
          "position": {
            "x": 0,
            "y": 0
          },
          "direction": "east"
        },
        "instructions": "A"
      }
END_INPUT

    assert_success
    expected='{"position":{"x":1,"y":0},"direction":"east"}'
    assert_objects_equal "$output" "$expected"
}

@test 'Moving forward one:facing west decrements X' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f robot-simulator.jq << 'END_INPUT'
      {
        "robot": {
          "position": {
            "x": 0,
            "y": 0
          },
          "direction": "west"
        },
        "instructions": "A"
      }
END_INPUT

    assert_success
    expected='{"position":{"x":-1,"y":0},"direction":"west"}'
    assert_objects_equal "$output" "$expected"
}

@test 'Follow series of instructions:moving east and north from README' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f robot-simulator.jq << 'END_INPUT'
      {
        "robot": {
          "position": {
            "x": 7,
            "y": 3
          },
          "direction": "north"
        },
        "instructions": "RAALAL"
      }
END_INPUT

    assert_success
    expected='{"position":{"x":9,"y":4},"direction":"west"}'
    assert_objects_equal "$output" "$expected"
}

@test 'Follow series of instructions:moving west and north' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f robot-simulator.jq << 'END_INPUT'
      {
        "robot": {
          "position": {
            "x": 0,
            "y": 0
          },
          "direction": "north"
        },
        "instructions": "LAAARALA"
      }
END_INPUT

    assert_success
    expected='{"position":{"x":-4,"y":1},"direction":"west"}'
    assert_objects_equal "$output" "$expected"
}

@test 'Follow series of instructions:moving west and south' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f robot-simulator.jq << 'END_INPUT'
      {
        "robot": {
          "position": {
            "x": 2,
            "y": -7
          },
          "direction": "east"
        },
        "instructions": "RRAAAAALA"
      }
END_INPUT

    assert_success
    expected='{"position":{"x":-3,"y":-8},"direction":"south"}'
    assert_objects_equal "$output" "$expected"
}

@test 'Follow series of instructions:moving east and north' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f robot-simulator.jq << 'END_INPUT'
      {
        "robot": {
          "position": {
            "x": 8,
            "y": 4
          },
          "direction": "south"
        },
        "instructions": "LAAARRRALLLL"
      }
END_INPUT

    assert_success
    expected='{"position":{"x":11,"y":5},"direction":"north"}'
    assert_objects_equal "$output" "$expected"
}
