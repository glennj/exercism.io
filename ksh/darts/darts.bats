#!/usr/bin/env bats

# local version: 2.2.0.1

@test "Missed target" {
    #[[ $BATS_RUN_SKIPPED == "true" ]] || skip
    run ksh darts -9 9
    (( status == 0 ))
    [[ $output == "0" ]]
}

@test "On the outer circle" {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip
    run ksh darts 0 10
    (( status == 0 ))
    [[ $output == "1" ]]
}

@test "On the middle circle" {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip
    run ksh darts -5 0
    (( status == 0 ))
    [[ $output == "5" ]]
}

@test "On the inner circle" {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip
    run ksh darts 0 -1
    (( status == 0 ))
    [[ $output == "10" ]]
}

@test "Exactly on centre" {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip
    run ksh darts 0 0
    (( status == 0 ))
    [[ $output == "10" ]]
}

@test "Near the centre" {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip
    run ksh darts -0.1 -0.1
    (( status == 0 ))
    [[ $output == "10" ]]
}

@test "Just within the inner circle" {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip
    run ksh darts 0.7 0.7
    (( status == 0 ))
    [[ $output == "10" ]]
}

@test "Just outside the inner circle" {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip
    run ksh darts 0.8 -0.8
    (( status == 0 ))
    [[ $output == "5" ]]
}

@test "Just within the middle circle" {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip
    run ksh darts -3.5 3.5
    (( status == 0 ))
    [[ $output == "5" ]]
}   

@test "Just outside the middle circle" {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip
    run ksh darts -3.6 -3.6
    (( status == 0 ))
    [[ $output == "1" ]]
}     

@test "Just within the outer circle" {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip
    run ksh darts -7.0 7.0
    (( status == 0 ))
    [[ $output == "1" ]]
}     

@test "Just outside the outer circle" {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip
    run ksh darts 7.1 -7.1
    (( status == 0 ))
    [[ $output == "0" ]]
}    

@test "Asymmetric position between the inner and middle circles" {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip
    run ksh darts 0.5 -4
    (( status == 0 ))
    [[ $output == "5" ]]
}

# ksh-specific test: Input validation

@test "invalid args: no args" {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip
    run ksh darts
    (( status == 1 ))
    [[  -n $output ]]
}

@test "invalid args: only 1 arg" {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip
    run ksh darts 10
    (( status == 1 ))
    [[  -n $output ]]
}

@test "invalid args: first arg non-numeric" {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip
    run ksh darts foo 10
    (( status == 1 ))
    [[  -n $output ]]
}

@test "invalid args: second arg non-numeric" {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip
    run ksh darts 10 bar
    (( status == 1 ))
    [[  -n $output ]]
}
