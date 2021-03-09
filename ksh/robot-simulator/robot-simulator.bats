#!/usr/bin/env bats
# vim: ft=bash

# local version: 0.1.0.0

# "Create robot",

@test "at origin facing north" {
    #[[ $BATS_RUN_SKIPPED == "true" ]] || skip
    run ksh robot-simulator
    [[ $status -eq 0 ]]
    [[ $output == '{"position": {"x": 0, "y": 0}, "direction": "north"}' ]]
}

@test "at negative position facing south" {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip
    run ksh robot-simulator -x -1 -y -1 -d south
    [[ $status -eq 0 ]]
    [[ $output == '{"position": {"x": -1, "y": -1}, "direction": "south"}' ]]
}

# "Rotating clockwise",

@test "changes north to east" {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip
    run ksh robot-simulator -x 0 -y 0 -d "north" "R"
    [[ $status -eq 0 ]]
    [[ $output == '{"position": {"x": 0, "y": 0}, "direction": "east"}' ]]
}

@test "changes east to south" {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip
    run ksh robot-simulator -x 0 -y 0 -d "east" "R"
    [[ $status -eq 0 ]]
    [[ $output == '{"position": {"x": 0, "y": 0}, "direction": "south"}' ]]
}

@test "changes south to west" {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip
    run ksh robot-simulator -x 0 -y 0 -d "south" "R"
    [[ $status -eq 0 ]]
    [[ $output == '{"position": {"x": 0, "y": 0}, "direction": "west"}' ]]
}

@test "changes west to north" {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip
    run ksh robot-simulator -x 0 -y 0 -d "west" "R"
    [[ $status -eq 0 ]]
    [[ $output == '{"position": {"x": 0, "y": 0}, "direction": "north"}' ]]
}

# "Rotating counter-clockwise",

@test "changes north to west" {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip
    run ksh robot-simulator -x 0 -y 0 -d "north" "L"
    [[ $status -eq 0 ]]
    [[ $output == '{"position": {"x": 0, "y": 0}, "direction": "west"}' ]]
}

@test "changes west to south" {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip
    run ksh robot-simulator -x 0 -y 0 -d "west" "L"
    [[ $status -eq 0 ]]
    [[ $output == '{"position": {"x": 0, "y": 0}, "direction": "south"}' ]]
}

@test "changes south to east" {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip
    run ksh robot-simulator -x 0 -y 0 -d "south" "L"
    [[ $status -eq 0 ]]
    [[ $output == '{"position": {"x": 0, "y": 0}, "direction": "east"}' ]]
}

@test "changes east to north" {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip
    run ksh robot-simulator -x 0 -y 0 -d "east" "L"
    [[ $status -eq 0 ]]
    [[ $output == '{"position": {"x": 0, "y": 0}, "direction": "north"}' ]]
}

# "Moving forward one",

@test "facing north increments Y" {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip
    run ksh robot-simulator -x 0 -y 0 -d "north" "A"
    [[ $status -eq 0 ]]
    [[ $output == '{"position": {"x": 0, "y": 1}, "direction": "north"}' ]]
}

@test "facing south decrements Y" {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip
    run ksh robot-simulator -x 0 -y 0 -d "south" "A"
    [[ $status -eq 0 ]]
    [[ $output == '{"position": {"x": 0, "y": -1}, "direction": "south"}' ]]
}

@test "facing east increments X" {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip
    run ksh robot-simulator -x 0 -y 0 -d "east" "A"
    [[ $status -eq 0 ]]
    [[ $output == '{"position": {"x": 1, "y": 0}, "direction": "east"}' ]]
}

@test "facing west decrements X" {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip
    run ksh robot-simulator -x 0 -y 0 -d "west" "A"
    [[ $status -eq 0 ]]
    [[ $output == '{"position": {"x": -1, "y": 0}, "direction": "west"}' ]]
}

# Follow series of instructions,
# The robot can follow a series of instructions and end up with the correct
# position and direction.
# Where R = Turn Right, L = Turn Left and A = Advance

@test "moving east and north from README" {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip
    run ksh robot-simulator -x 7 -y 3 -d "north" "RAALAL"
    [[ $status -eq 0 ]]
    [[ $output == '{"position": {"x": 9, "y": 4}, "direction": "west"}' ]]
}

@test "moving west and north" {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip
    run ksh robot-simulator -x 0 -y 0 -d "north" "LAAARALA"
    [[ $status -eq 0 ]]
    [[ $output == '{"position": {"x": -4, "y": 1}, "direction": "west"}' ]]
}

@test "moving west and south" {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip
    run ksh robot-simulator -x 2 -y -7 -d "east" "RRAAAAALA"
    [[ $status -eq 0 ]]
    [[ $output == '{"position": {"x": -3, "y": -8}, "direction": "south"}' ]]
}

@test "moving east and north" {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip
    run ksh robot-simulator -x 8 -y 4 -d "south" "LAAARRRALLLL"
    [[ $status -eq 0 ]]
    [[ $output == '{"position": {"x": 11, "y": 5}, "direction": "north"}' ]]
}

