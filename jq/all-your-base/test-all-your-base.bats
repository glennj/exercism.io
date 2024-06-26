#!/usr/bin/env bats
# generated on 
load bats-extra
load bats-jq

@test 'single bit one to decimal' {
    #[[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f all-your-base.jq << 'END_INPUT'
        {
          "inputBase": 2,
          "digits": [1],
          "outputBase": 10
        }
END_INPUT

    assert_success
    expected='[1]'
    assert_equal "$output" "$expected"
}

@test 'binary to single decimal' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f all-your-base.jq << 'END_INPUT'
        {
          "inputBase": 2,
          "digits": [1, 0, 1],
          "outputBase": 10
        }
END_INPUT

    assert_success
    expected='[5]'
    assert_equal "$output" "$expected"
}

@test 'single decimal to binary' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f all-your-base.jq << 'END_INPUT'
        {
          "inputBase": 10,
          "digits": [5],
          "outputBase": 2
        }
END_INPUT

    assert_success
    expected='[1,0,1]'
    assert_equal "$output" "$expected"
}

@test 'binary to multiple decimal' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f all-your-base.jq << 'END_INPUT'
        {
          "inputBase": 2,
          "digits": [1, 0, 1, 0, 1, 0],
          "outputBase": 10
        }
END_INPUT

    assert_success
    expected='[4,2]'
    assert_equal "$output" "$expected"
}

@test 'decimal to binary' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f all-your-base.jq << 'END_INPUT'
        {
          "inputBase": 10,
          "digits": [4, 2],
          "outputBase": 2
        }
END_INPUT

    assert_success
    expected='[1,0,1,0,1,0]'
    assert_equal "$output" "$expected"
}

@test 'trinary to hexadecimal' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f all-your-base.jq << 'END_INPUT'
        {
          "inputBase": 3,
          "digits": [1, 1, 2, 0],
          "outputBase": 16
        }
END_INPUT

    assert_success
    expected='[2,10]'
    assert_equal "$output" "$expected"
}

@test 'hexadecimal to trinary' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f all-your-base.jq << 'END_INPUT'
        {
          "inputBase": 16,
          "digits": [2, 10],
          "outputBase": 3
        }
END_INPUT

    assert_success
    expected='[1,1,2,0]'
    assert_equal "$output" "$expected"
}

@test '15-bit integer' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f all-your-base.jq << 'END_INPUT'
        {
          "inputBase": 97,
          "digits": [3, 46, 60],
          "outputBase": 73
        }
END_INPUT

    assert_success
    expected='[6,10,45]'
    assert_equal "$output" "$expected"
}

@test 'empty list' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f all-your-base.jq << 'END_INPUT'
        {
          "inputBase": 2,
          "digits": [],
          "outputBase": 10
        }
END_INPUT

    assert_success
    expected='[0]'
    assert_equal "$output" "$expected"
}

@test 'single zero' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f all-your-base.jq << 'END_INPUT'
        {
          "inputBase": 10,
          "digits": [0],
          "outputBase": 2
        }
END_INPUT

    assert_success
    expected='[0]'
    assert_equal "$output" "$expected"
}

@test 'multiple zeros' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f all-your-base.jq << 'END_INPUT'
        {
          "inputBase": 10,
          "digits": [0, 0, 0],
          "outputBase": 2
        }
END_INPUT

    assert_success
    expected='[0]'
    assert_equal "$output" "$expected"
}

@test 'leading zeros' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f all-your-base.jq << 'END_INPUT'
        {
          "inputBase": 7,
          "digits": [0, 6, 0],
          "outputBase": 10
        }
END_INPUT

    assert_success
    expected='[4,2]'
    assert_equal "$output" "$expected"
}

@test 'input base is one' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f all-your-base.jq << 'END_INPUT'
        {
          "inputBase": 1,
          "digits": [0],
          "outputBase": 10
        }
END_INPUT

    assert_failure
    expected='input base must be >= 2'
    assert_equal "$output" "$expected"
}

@test 'input base is zero' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f all-your-base.jq << 'END_INPUT'
        {
          "inputBase": 0,
          "digits": [],
          "outputBase": 10
        }
END_INPUT

    assert_failure
    expected='input base must be >= 2'
    assert_equal "$output" "$expected"
}

@test 'input base is negative' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f all-your-base.jq << 'END_INPUT'
        {
          "inputBase": -2,
          "digits": [1],
          "outputBase": 10
        }
END_INPUT

    assert_failure
    expected='input base must be >= 2'
    assert_equal "$output" "$expected"
}

@test 'negative digit' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f all-your-base.jq << 'END_INPUT'
        {
          "inputBase": 2,
          "digits": [1, -1, 1, 0, 1, 0],
          "outputBase": 10
        }
END_INPUT

    assert_failure
    expected='all digits must satisfy 0 <= d < input base'
    assert_equal "$output" "$expected"
}

@test 'invalid positive digit' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f all-your-base.jq << 'END_INPUT'
        {
          "inputBase": 2,
          "digits": [1, 2, 1, 0, 1, 0],
          "outputBase": 10
        }
END_INPUT

    assert_failure
    expected='all digits must satisfy 0 <= d < input base'
    assert_equal "$output" "$expected"
}

@test 'output base is one' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f all-your-base.jq << 'END_INPUT'
        {
          "inputBase": 2,
          "digits": [1, 0, 1, 0, 1, 0],
          "outputBase": 1
        }
END_INPUT

    assert_failure
    expected='output base must be >= 2'
    assert_equal "$output" "$expected"
}

@test 'output base is zero' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f all-your-base.jq << 'END_INPUT'
        {
          "inputBase": 10,
          "digits": [7],
          "outputBase": 0
        }
END_INPUT

    assert_failure
    expected='output base must be >= 2'
    assert_equal "$output" "$expected"
}

@test 'output base is negative' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f all-your-base.jq << 'END_INPUT'
        {
          "inputBase": 2,
          "digits": [1],
          "outputBase": -7
        }
END_INPUT

    assert_failure
    expected='output base must be >= 2'
    assert_equal "$output" "$expected"
}

@test 'both bases are negative' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f all-your-base.jq << 'END_INPUT'
        {
          "inputBase": -2,
          "digits": [1],
          "outputBase": -7
        }
END_INPUT

    assert_failure
    expected='input base must be >= 2'
    assert_equal "$output" "$expected"
}
