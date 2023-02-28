#!/usr/bin/env bats
# generated on 2022-11-02T20:59:07Z
load bats-extra

@test 'parsing and numbers:numbers just get pushed onto the stack' {
    #[[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f forth.jq << 'END_INPUT'
        {
          "instructions": [
            "1 2 3 4 5"
          ]
        }
END_INPUT

    assert_success
    expected='[1,2,3,4,5]'
    assert_equal "$output" "$expected"
}

@test 'parsing and numbers:pushes negative numbers onto the stack' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f forth.jq << 'END_INPUT'
        {
          "instructions": [
            "-1 -2 -3 -4 -5"
          ]
        }
END_INPUT

    assert_success
    expected='[-1,-2,-3,-4,-5]'
    assert_equal "$output" "$expected"
}

@test 'addition:can add two numbers' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f forth.jq << 'END_INPUT'
        {
          "instructions": [
            "1 2 +"
          ]
        }
END_INPUT

    assert_success
    expected='[3]'
    assert_equal "$output" "$expected"
}

@test 'addition:errors if there is nothing on the stack' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f forth.jq << 'END_INPUT'
        {
          "instructions": [
            "+"
          ]
        }
END_INPUT

    assert_failure
    expected='empty stack'
    assert_equal "$output" "$expected"
}

@test 'addition:errors if there is only one value on the stack' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f forth.jq << 'END_INPUT'
        {
          "instructions": [
            "1 +"
          ]
        }
END_INPUT

    assert_failure
    expected='only one value on the stack'
    assert_equal "$output" "$expected"
}

@test 'subtraction:can subtract two numbers' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f forth.jq << 'END_INPUT'
        {
          "instructions": [
            "3 4 -"
          ]
        }
END_INPUT

    assert_success
    expected='[-1]'
    assert_equal "$output" "$expected"
}

@test 'subtraction:errors if there is nothing on the stack' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f forth.jq << 'END_INPUT'
        {
          "instructions": [
            "-"
          ]
        }
END_INPUT

    assert_failure
    expected='empty stack'
    assert_equal "$output" "$expected"
}

@test 'subtraction:errors if there is only one value on the stack' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f forth.jq << 'END_INPUT'
        {
          "instructions": [
            "1 -"
          ]
        }
END_INPUT

    assert_failure
    expected='only one value on the stack'
    assert_equal "$output" "$expected"
}

@test 'multiplication:can multiply two numbers' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f forth.jq << 'END_INPUT'
        {
          "instructions": [
            "2 4 *"
          ]
        }
END_INPUT

    assert_success
    expected='[8]'
    assert_equal "$output" "$expected"
}

@test 'multiplication:errors if there is nothing on the stack' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f forth.jq << 'END_INPUT'
        {
          "instructions": [
            "*"
          ]
        }
END_INPUT

    assert_failure
    expected='empty stack'
    assert_equal "$output" "$expected"
}

@test 'multiplication:errors if there is only one value on the stack' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f forth.jq << 'END_INPUT'
        {
          "instructions": [
            "1 *"
          ]
        }
END_INPUT

    assert_failure
    expected='only one value on the stack'
    assert_equal "$output" "$expected"
}

@test 'division:can divide two numbers' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f forth.jq << 'END_INPUT'
        {
          "instructions": [
            "12 3 /"
          ]
        }
END_INPUT

    assert_success
    expected='[4]'
    assert_equal "$output" "$expected"
}

@test 'division:performs integer division' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f forth.jq << 'END_INPUT'
        {
          "instructions": [
            "8 3 /"
          ]
        }
END_INPUT

    assert_success
    expected='[2]'
    assert_equal "$output" "$expected"
}

@test 'division:errors if dividing by zero' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f forth.jq << 'END_INPUT'
        {
          "instructions": [
            "4 0 /"
          ]
        }
END_INPUT

    assert_failure
    expected='divide by zero'
    assert_equal "$output" "$expected"
}

@test 'division:errors if there is nothing on the stack' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f forth.jq << 'END_INPUT'
        {
          "instructions": [
            "/"
          ]
        }
END_INPUT

    assert_failure
    expected='empty stack'
    assert_equal "$output" "$expected"
}

@test 'division:errors if there is only one value on the stack' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f forth.jq << 'END_INPUT'
        {
          "instructions": [
            "1 /"
          ]
        }
END_INPUT

    assert_failure
    expected='only one value on the stack'
    assert_equal "$output" "$expected"
}

@test 'combined arithmetic:addition and subtraction' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f forth.jq << 'END_INPUT'
        {
          "instructions": [
            "1 2 + 4 -"
          ]
        }
END_INPUT

    assert_success
    expected='[-1]'
    assert_equal "$output" "$expected"
}

@test 'combined arithmetic:multiplication and division' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f forth.jq << 'END_INPUT'
        {
          "instructions": [
            "2 4 * 3 /"
          ]
        }
END_INPUT

    assert_success
    expected='[2]'
    assert_equal "$output" "$expected"
}

@test 'dup:copies a value on the stack' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f forth.jq << 'END_INPUT'
        {
          "instructions": [
            "1 dup"
          ]
        }
END_INPUT

    assert_success
    expected='[1,1]'
    assert_equal "$output" "$expected"
}

@test 'dup:copies the top value on the stack' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f forth.jq << 'END_INPUT'
        {
          "instructions": [
            "1 2 dup"
          ]
        }
END_INPUT

    assert_success
    expected='[1,2,2]'
    assert_equal "$output" "$expected"
}

@test 'dup:errors if there is nothing on the stack' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f forth.jq << 'END_INPUT'
        {
          "instructions": [
            "dup"
          ]
        }
END_INPUT

    assert_failure
    expected='empty stack'
    assert_equal "$output" "$expected"
}

@test 'drop:removes the top value on the stack if it is the only one' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f forth.jq << 'END_INPUT'
        {
          "instructions": [
            "1 drop"
          ]
        }
END_INPUT

    assert_success
    expected='[]'
    assert_equal "$output" "$expected"
}

@test 'drop:removes the top value on the stack if it is not the only one' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f forth.jq << 'END_INPUT'
        {
          "instructions": [
            "1 2 drop"
          ]
        }
END_INPUT

    assert_success
    expected='[1]'
    assert_equal "$output" "$expected"
}

@test 'drop:errors if there is nothing on the stack' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f forth.jq << 'END_INPUT'
        {
          "instructions": [
            "drop"
          ]
        }
END_INPUT

    assert_failure
    expected='empty stack'
    assert_equal "$output" "$expected"
}

@test 'swap:swaps the top two values on the stack if they are the only ones' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f forth.jq << 'END_INPUT'
        {
          "instructions": [
            "1 2 swap"
          ]
        }
END_INPUT

    assert_success
    expected='[2,1]'
    assert_equal "$output" "$expected"
}

@test 'swap:swaps the top two values on the stack if they are not the only ones' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f forth.jq << 'END_INPUT'
        {
          "instructions": [
            "1 2 3 swap"
          ]
        }
END_INPUT

    assert_success
    expected='[1,3,2]'
    assert_equal "$output" "$expected"
}

@test 'swap:errors if there is nothing on the stack' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f forth.jq << 'END_INPUT'
        {
          "instructions": [
            "swap"
          ]
        }
END_INPUT

    assert_failure
    expected='empty stack'
    assert_equal "$output" "$expected"
}

@test 'swap:errors if there is only one value on the stack' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f forth.jq << 'END_INPUT'
        {
          "instructions": [
            "1 swap"
          ]
        }
END_INPUT

    assert_failure
    expected='only one value on the stack'
    assert_equal "$output" "$expected"
}

@test 'over:copies the second element if there are only two' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f forth.jq << 'END_INPUT'
        {
          "instructions": [
            "1 2 over"
          ]
        }
END_INPUT

    assert_success
    expected='[1,2,1]'
    assert_equal "$output" "$expected"
}

@test 'over:copies the second element if there are more than two' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f forth.jq << 'END_INPUT'
        {
          "instructions": [
            "1 2 3 over"
          ]
        }
END_INPUT

    assert_success
    expected='[1,2,3,2]'
    assert_equal "$output" "$expected"
}

@test 'over:errors if there is nothing on the stack' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f forth.jq << 'END_INPUT'
        {
          "instructions": [
            "over"
          ]
        }
END_INPUT

    assert_failure
    expected='empty stack'
    assert_equal "$output" "$expected"
}

@test 'over:errors if there is only one value on the stack' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f forth.jq << 'END_INPUT'
        {
          "instructions": [
            "1 over"
          ]
        }
END_INPUT

    assert_failure
    expected='only one value on the stack'
    assert_equal "$output" "$expected"
}

@test 'user-defined words:can consist of built-in words' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f forth.jq << 'END_INPUT'
        {
          "instructions": [
            ": dup-twice dup dup ;",
            "1 dup-twice"
          ]
        }
END_INPUT

    assert_success
    expected='[1,1,1]'
    assert_equal "$output" "$expected"
}

@test 'user-defined words:execute in the right order' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f forth.jq << 'END_INPUT'
        {
          "instructions": [
            ": countup 1 2 3 ;",
            "countup"
          ]
        }
END_INPUT

    assert_success
    expected='[1,2,3]'
    assert_equal "$output" "$expected"
}

@test 'user-defined words:can override other user-defined words' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f forth.jq << 'END_INPUT'
        {
          "instructions": [
            ": foo dup ;",
            ": foo dup dup ;",
            "1 foo"
          ]
        }
END_INPUT

    assert_success
    expected='[1,1,1]'
    assert_equal "$output" "$expected"
}

@test 'user-defined words:can override built-in words' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f forth.jq << 'END_INPUT'
        {
          "instructions": [
            ": swap dup ;",
            "1 swap"
          ]
        }
END_INPUT

    assert_success
    expected='[1,1]'
    assert_equal "$output" "$expected"
}

@test 'user-defined words:can override built-in operators' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f forth.jq << 'END_INPUT'
        {
          "instructions": [
            ": + * ;",
            "3 4 +"
          ]
        }
END_INPUT

    assert_success
    expected='[12]'
    assert_equal "$output" "$expected"
}

@test 'user-defined words:can use different words with the same name' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f forth.jq << 'END_INPUT'
        {
          "instructions": [
            ": foo 5 ;",
            ": bar foo ;",
            ": foo 6 ;",
            "bar foo"
          ]
        }
END_INPUT

    assert_success
    expected='[5,6]'
    assert_equal "$output" "$expected"
}

@test 'user-defined words:can define word that uses word with the same name' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f forth.jq << 'END_INPUT'
        {
          "instructions": [
            ": foo 10 ;",
            ": foo foo 1 + ;",
            "foo"
          ]
        }
END_INPUT

    assert_success
    expected='[11]'
    assert_equal "$output" "$expected"
}

@test 'user-defined words:cannot redefine non-negative numbers' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f forth.jq << 'END_INPUT'
        {
          "instructions": [
            ": 1 2 ;"
          ]
        }
END_INPUT

    assert_failure
    expected='illegal operation'
    assert_equal "$output" "$expected"
}

@test 'user-defined words:cannot redefine negative numbers' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f forth.jq << 'END_INPUT'
        {
          "instructions": [
            ": -1 2 ;"
          ]
        }
END_INPUT

    assert_failure
    expected='illegal operation'
    assert_equal "$output" "$expected"
}

@test 'user-defined words:errors if executing a non-existent word' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f forth.jq << 'END_INPUT'
        {
          "instructions": [
            "foo"
          ]
        }
END_INPUT

    assert_failure
    expected='undefined operation'
    assert_equal "$output" "$expected"
}

@test 'case-insensitivity:DUP is case-insensitive' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f forth.jq << 'END_INPUT'
        {
          "instructions": [
            "1 DUP Dup dup"
          ]
        }
END_INPUT

    assert_success
    expected='[1,1,1,1]'
    assert_equal "$output" "$expected"
}

@test 'case-insensitivity:DROP is case-insensitive' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f forth.jq << 'END_INPUT'
        {
          "instructions": [
            "1 2 3 4 DROP Drop drop"
          ]
        }
END_INPUT

    assert_success
    expected='[1]'
    assert_equal "$output" "$expected"
}

@test 'case-insensitivity:SWAP is case-insensitive' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f forth.jq << 'END_INPUT'
        {
          "instructions": [
            "1 2 SWAP 3 Swap 4 swap"
          ]
        }
END_INPUT

    assert_success
    expected='[2,3,4,1]'
    assert_equal "$output" "$expected"
}

@test 'case-insensitivity:OVER is case-insensitive' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f forth.jq << 'END_INPUT'
        {
          "instructions": [
            "1 2 OVER Over over"
          ]
        }
END_INPUT

    assert_success
    expected='[1,2,1,2,1]'
    assert_equal "$output" "$expected"
}

@test 'case-insensitivity:user-defined words are case-insensitive' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f forth.jq << 'END_INPUT'
        {
          "instructions": [
            ": foo dup ;",
            "1 FOO Foo foo"
          ]
        }
END_INPUT

    assert_success
    expected='[1,1,1,1]'
    assert_equal "$output" "$expected"
}

@test 'case-insensitivity:definitions are case-insensitive' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f forth.jq << 'END_INPUT'
        {
          "instructions": [
            ": SWAP DUP Dup dup ;",
            "1 swap"
          ]
        }
END_INPUT

    assert_success
    expected='[1,1,1,1]'
    assert_equal "$output" "$expected"
}

