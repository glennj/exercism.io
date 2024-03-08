#!/usr/bin/env bats
# generated on 2024-02-21T23:55:25Z
load bats-extra

@test 'Square the sum of the numbers up to the given number:square of sum 1' {
    #[[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f difference-of-squares.jq << 'END_INPUT'
        {
          "property": "squareOfSum",
          "input": {
            "number": 1
          }
        }
END_INPUT

    assert_success
    expected=1
    assert_equal "$output" "$expected"
}

@test 'Square the sum of the numbers up to the given number:square of sum 5' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f difference-of-squares.jq << 'END_INPUT'
        {
          "property": "squareOfSum",
          "input": {
            "number": 5
          }
        }
END_INPUT

    assert_success
    expected=225
    assert_equal "$output" "$expected"
}

@test 'Square the sum of the numbers up to the given number:square of sum 100' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f difference-of-squares.jq << 'END_INPUT'
        {
          "property": "squareOfSum",
          "input": {
            "number": 100
          }
        }
END_INPUT

    assert_success
    expected=25502500
    assert_equal "$output" "$expected"
}

@test 'Sum the squares of the numbers up to the given number:sum of squares 1' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f difference-of-squares.jq << 'END_INPUT'
        {
          "property": "sumOfSquares",
          "input": {
            "number": 1
          }
        }
END_INPUT

    assert_success
    expected=1
    assert_equal "$output" "$expected"
}

@test 'Sum the squares of the numbers up to the given number:sum of squares 5' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f difference-of-squares.jq << 'END_INPUT'
        {
          "property": "sumOfSquares",
          "input": {
            "number": 5
          }
        }
END_INPUT

    assert_success
    expected=55
    assert_equal "$output" "$expected"
}

@test 'Sum the squares of the numbers up to the given number:sum of squares 100' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f difference-of-squares.jq << 'END_INPUT'
        {
          "property": "sumOfSquares",
          "input": {
            "number": 100
          }
        }
END_INPUT

    assert_success
    expected=338350
    assert_equal "$output" "$expected"
}

@test 'Subtract sum of squares from square of sums:difference of squares 1' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f difference-of-squares.jq << 'END_INPUT'
        {
          "property": "differenceOfSquares",
          "input": {
            "number": 1
          }
        }
END_INPUT

    assert_success
    expected=0
    assert_equal "$output" "$expected"
}

@test 'Subtract sum of squares from square of sums:difference of squares 5' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f difference-of-squares.jq << 'END_INPUT'
        {
          "property": "differenceOfSquares",
          "input": {
            "number": 5
          }
        }
END_INPUT

    assert_success
    expected=170
    assert_equal "$output" "$expected"
}

@test 'Subtract sum of squares from square of sums:difference of squares 100' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f difference-of-squares.jq << 'END_INPUT'
        {
          "property": "differenceOfSquares",
          "input": {
            "number": 100
          }
        }
END_INPUT

    assert_success
    expected=25164150
    assert_equal "$output" "$expected"
}
