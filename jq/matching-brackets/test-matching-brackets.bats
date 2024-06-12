#!/usr/bin/env bats
# generated on 
load bats-extra

@test 'paired square brackets' {
    #[[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f matching-brackets.jq << 'END_INPUT'
        {
          "value": "[]"
        }
END_INPUT

    assert_success
    expected=true
    assert_equal "$output" "$expected"
}

@test 'empty string' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f matching-brackets.jq << 'END_INPUT'
        {
          "value": ""
        }
END_INPUT

    assert_success
    expected=true
    assert_equal "$output" "$expected"
}

@test 'unpaired brackets' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f matching-brackets.jq << 'END_INPUT'
        {
          "value": "[["
        }
END_INPUT

    assert_success
    expected=false
    assert_equal "$output" "$expected"
}

@test 'wrong ordered brackets' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f matching-brackets.jq << 'END_INPUT'
        {
          "value": "}{"
        }
END_INPUT

    assert_success
    expected=false
    assert_equal "$output" "$expected"
}

@test 'wrong closing bracket' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f matching-brackets.jq << 'END_INPUT'
        {
          "value": "{]"
        }
END_INPUT

    assert_success
    expected=false
    assert_equal "$output" "$expected"
}

@test 'paired with whitespace' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f matching-brackets.jq << 'END_INPUT'
        {
          "value": "{ }"
        }
END_INPUT

    assert_success
    expected=true
    assert_equal "$output" "$expected"
}

@test 'partially paired brackets' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f matching-brackets.jq << 'END_INPUT'
        {
          "value": "{[])"
        }
END_INPUT

    assert_success
    expected=false
    assert_equal "$output" "$expected"
}

@test 'simple nested brackets' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f matching-brackets.jq << 'END_INPUT'
        {
          "value": "{[]}"
        }
END_INPUT

    assert_success
    expected=true
    assert_equal "$output" "$expected"
}

@test 'several paired brackets' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f matching-brackets.jq << 'END_INPUT'
        {
          "value": "{}[]"
        }
END_INPUT

    assert_success
    expected=true
    assert_equal "$output" "$expected"
}

@test 'paired and nested brackets' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f matching-brackets.jq << 'END_INPUT'
        {
          "value": "([{}({}[])])"
        }
END_INPUT

    assert_success
    expected=true
    assert_equal "$output" "$expected"
}

@test 'unopened closing brackets' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f matching-brackets.jq << 'END_INPUT'
        {
          "value": "{[)][]}"
        }
END_INPUT

    assert_success
    expected=false
    assert_equal "$output" "$expected"
}

@test 'unpaired and nested brackets' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f matching-brackets.jq << 'END_INPUT'
        {
          "value": "([{])"
        }
END_INPUT

    assert_success
    expected=false
    assert_equal "$output" "$expected"
}

@test 'paired and wrong nested brackets' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f matching-brackets.jq << 'END_INPUT'
        {
          "value": "[({]})"
        }
END_INPUT

    assert_success
    expected=false
    assert_equal "$output" "$expected"
}

@test 'paired and wrong nested brackets but innermost are correct' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f matching-brackets.jq << 'END_INPUT'
        {
          "value": "[({}])"
        }
END_INPUT

    assert_success
    expected=false
    assert_equal "$output" "$expected"
}

@test 'paired and incomplete brackets' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f matching-brackets.jq << 'END_INPUT'
        {
          "value": "{}["
        }
END_INPUT

    assert_success
    expected=false
    assert_equal "$output" "$expected"
}

@test 'too many closing brackets' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f matching-brackets.jq << 'END_INPUT'
        {
          "value": "[]]"
        }
END_INPUT

    assert_success
    expected=false
    assert_equal "$output" "$expected"
}

@test 'early unexpected brackets' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f matching-brackets.jq << 'END_INPUT'
        {
          "value": ")()"
        }
END_INPUT

    assert_success
    expected=false
    assert_equal "$output" "$expected"
}

@test 'early mismatched brackets' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f matching-brackets.jq << 'END_INPUT'
        {
          "value": "{)()"
        }
END_INPUT

    assert_success
    expected=false
    assert_equal "$output" "$expected"
}

@test 'math expression' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f matching-brackets.jq << 'END_INPUT'
        {
          "value": "(((185 + 223.85) * 15) - 543)/2"
        }
END_INPUT

    assert_success
    expected=true
    assert_equal "$output" "$expected"
}

@test 'complex latex expression' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f matching-brackets.jq << 'END_INPUT'
        {
          "value": "\\left(\\begin{array}{cc} \\frac{1}{3} & x\\\\ \\mathrm{e}^{x} &... x^2 \\end{array}\\right)"
        }
END_INPUT

    assert_success
    expected=true
    assert_equal "$output" "$expected"
}
