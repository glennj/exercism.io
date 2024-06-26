#!/usr/bin/env bats
# generated on 2022-11-02T20:59:38Z
load bats-extra
load bats-jq

@test 'the sound for 1 is 1' {
    #[[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f raindrops.jq << 'END_INPUT'
        {
          "number": 1
        }
END_INPUT

    assert_success
    expected='1'
    assert_equal "$output" "$expected"
}

@test 'the sound for 3 is Pling' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f raindrops.jq << 'END_INPUT'
        {
          "number": 3
        }
END_INPUT

    assert_success
    expected='Pling'
    assert_equal "$output" "$expected"
}

@test 'the sound for 5 is Plang' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f raindrops.jq << 'END_INPUT'
        {
          "number": 5
        }
END_INPUT

    assert_success
    expected='Plang'
    assert_equal "$output" "$expected"
}

@test 'the sound for 7 is Plong' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f raindrops.jq << 'END_INPUT'
        {
          "number": 7
        }
END_INPUT

    assert_success
    expected='Plong'
    assert_equal "$output" "$expected"
}

@test 'the sound for 6 is Pling as it has a factor 3' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f raindrops.jq << 'END_INPUT'
        {
          "number": 6
        }
END_INPUT

    assert_success
    expected='Pling'
    assert_equal "$output" "$expected"
}

@test '2 to the power 3 does not make a raindrop sound as 3 is the exponent not the base' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f raindrops.jq << 'END_INPUT'
        {
          "number": 8
        }
END_INPUT

    assert_success
    expected='8'
    assert_equal "$output" "$expected"
}

@test 'the sound for 9 is Pling as it has a factor 3' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f raindrops.jq << 'END_INPUT'
        {
          "number": 9
        }
END_INPUT

    assert_success
    expected='Pling'
    assert_equal "$output" "$expected"
}

@test 'the sound for 10 is Plang as it has a factor 5' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f raindrops.jq << 'END_INPUT'
        {
          "number": 10
        }
END_INPUT

    assert_success
    expected='Plang'
    assert_equal "$output" "$expected"
}

@test 'the sound for 14 is Plong as it has a factor of 7' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f raindrops.jq << 'END_INPUT'
        {
          "number": 14
        }
END_INPUT

    assert_success
    expected='Plong'
    assert_equal "$output" "$expected"
}

@test 'the sound for 15 is PlingPlang as it has factors 3 and 5' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f raindrops.jq << 'END_INPUT'
        {
          "number": 15
        }
END_INPUT

    assert_success
    expected='PlingPlang'
    assert_equal "$output" "$expected"
}

@test 'the sound for 21 is PlingPlong as it has factors 3 and 7' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f raindrops.jq << 'END_INPUT'
        {
          "number": 21
        }
END_INPUT

    assert_success
    expected='PlingPlong'
    assert_equal "$output" "$expected"
}

@test 'the sound for 25 is Plang as it has a factor 5' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f raindrops.jq << 'END_INPUT'
        {
          "number": 25
        }
END_INPUT

    assert_success
    expected='Plang'
    assert_equal "$output" "$expected"
}

@test 'the sound for 27 is Pling as it has a factor 3' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f raindrops.jq << 'END_INPUT'
        {
          "number": 27
        }
END_INPUT

    assert_success
    expected='Pling'
    assert_equal "$output" "$expected"
}

@test 'the sound for 35 is PlangPlong as it has factors 5 and 7' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f raindrops.jq << 'END_INPUT'
        {
          "number": 35
        }
END_INPUT

    assert_success
    expected='PlangPlong'
    assert_equal "$output" "$expected"
}

@test 'the sound for 49 is Plong as it has a factor 7' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f raindrops.jq << 'END_INPUT'
        {
          "number": 49
        }
END_INPUT

    assert_success
    expected='Plong'
    assert_equal "$output" "$expected"
}

@test 'the sound for 52 is 52' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f raindrops.jq << 'END_INPUT'
        {
          "number": 52
        }
END_INPUT

    assert_success
    expected='52'
    assert_equal "$output" "$expected"
}

@test 'the sound for 105 is PlingPlangPlong as it has factors 3, 5 and 7' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f raindrops.jq << 'END_INPUT'
        {
          "number": 105
        }
END_INPUT

    assert_success
    expected='PlingPlangPlong'
    assert_equal "$output" "$expected"
}

@test 'the sound for 3125 is Plang as it has a factor 5' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f raindrops.jq << 'END_INPUT'
        {
          "number": 3125
        }
END_INPUT

    assert_success
    expected='Plang'
    assert_equal "$output" "$expected"
}

