#!/usr/bin/env bats
# generated on 2022-11-02T20:59:44Z
load bats-extra

@test 'run-length encode a string:empty string' {
    #[[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r '
                include "./run-length-encoding";
                if .property == "encode" then
                    .input.string | encode
                elif .property == "decode" then
                    .input.string | decode
                else
                    .input.string | encode | decode
                end
    ' << 'END_INPUT'
        {
          "property": "encode",
          "input": {
            "string": ""
          }
        }
END_INPUT

    assert_success
    expected=''
    assert_equal "$output" "$expected"
}

@test 'run-length encode a string:single characters only are encoded without count' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r '
                include "./run-length-encoding";
                if .property == "encode" then
                    .input.string | encode
                elif .property == "decode" then
                    .input.string | decode
                else
                    .input.string | encode | decode
                end
    ' << 'END_INPUT'
        {
          "property": "encode",
          "input": {
            "string": "XYZ"
          }
        }
END_INPUT

    assert_success
    expected='XYZ'
    assert_equal "$output" "$expected"
}

@test 'run-length encode a string:string with no single characters' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r '
                include "./run-length-encoding";
                if .property == "encode" then
                    .input.string | encode
                elif .property == "decode" then
                    .input.string | decode
                else
                    .input.string | encode | decode
                end
    ' << 'END_INPUT'
        {
          "property": "encode",
          "input": {
            "string": "AABBBCCCC"
          }
        }
END_INPUT

    assert_success
    expected='2A3B4C'
    assert_equal "$output" "$expected"
}

@test 'run-length encode a string:single characters mixed with repeated characters' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r '
                include "./run-length-encoding";
                if .property == "encode" then
                    .input.string | encode
                elif .property == "decode" then
                    .input.string | decode
                else
                    .input.string | encode | decode
                end
    ' << 'END_INPUT'
        {
          "property": "encode",
          "input": {
            "string": "WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWB"
          }
        }
END_INPUT

    assert_success
    expected='12WB12W3B24WB'
    assert_equal "$output" "$expected"
}

@test 'run-length encode a string:multiple whitespace mixed in string' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r '
                include "./run-length-encoding";
                if .property == "encode" then
                    .input.string | encode
                elif .property == "decode" then
                    .input.string | decode
                else
                    .input.string | encode | decode
                end
    ' << 'END_INPUT'
        {
          "property": "encode",
          "input": {
            "string": "  hsqq qww  "
          }
        }
END_INPUT

    assert_success
    expected='2 hs2q q2w2 '
    assert_equal "$output" "$expected"
}

@test 'run-length encode a string:lowercase characters' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r '
                include "./run-length-encoding";
                if .property == "encode" then
                    .input.string | encode
                elif .property == "decode" then
                    .input.string | decode
                else
                    .input.string | encode | decode
                end
    ' << 'END_INPUT'
        {
          "property": "encode",
          "input": {
            "string": "aabbbcccc"
          }
        }
END_INPUT

    assert_success
    expected='2a3b4c'
    assert_equal "$output" "$expected"
}

@test 'run-length decode a string:empty string' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r '
                include "./run-length-encoding";
                if .property == "encode" then
                    .input.string | encode
                elif .property == "decode" then
                    .input.string | decode
                else
                    .input.string | encode | decode
                end
    ' << 'END_INPUT'
        {
          "property": "decode",
          "input": {
            "string": ""
          }
        }
END_INPUT

    assert_success
    expected=''
    assert_equal "$output" "$expected"
}

@test 'run-length decode a string:single characters only' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r '
                include "./run-length-encoding";
                if .property == "encode" then
                    .input.string | encode
                elif .property == "decode" then
                    .input.string | decode
                else
                    .input.string | encode | decode
                end
    ' << 'END_INPUT'
        {
          "property": "decode",
          "input": {
            "string": "XYZ"
          }
        }
END_INPUT

    assert_success
    expected='XYZ'
    assert_equal "$output" "$expected"
}

@test 'run-length decode a string:string with no single characters' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r '
                include "./run-length-encoding";
                if .property == "encode" then
                    .input.string | encode
                elif .property == "decode" then
                    .input.string | decode
                else
                    .input.string | encode | decode
                end
    ' << 'END_INPUT'
        {
          "property": "decode",
          "input": {
            "string": "2A3B4C"
          }
        }
END_INPUT

    assert_success
    expected='AABBBCCCC'
    assert_equal "$output" "$expected"
}

@test 'run-length decode a string:single characters with repeated characters' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r '
                include "./run-length-encoding";
                if .property == "encode" then
                    .input.string | encode
                elif .property == "decode" then
                    .input.string | decode
                else
                    .input.string | encode | decode
                end
    ' << 'END_INPUT'
        {
          "property": "decode",
          "input": {
            "string": "12WB12W3B24WB"
          }
        }
END_INPUT

    assert_success
    expected='WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWB'
    assert_equal "$output" "$expected"
}

@test 'run-length decode a string:multiple whitespace mixed in string' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r '
                include "./run-length-encoding";
                if .property == "encode" then
                    .input.string | encode
                elif .property == "decode" then
                    .input.string | decode
                else
                    .input.string | encode | decode
                end
    ' << 'END_INPUT'
        {
          "property": "decode",
          "input": {
            "string": "2 hs2q q2w2 "
          }
        }
END_INPUT

    assert_success
    expected='  hsqq qww  '
    assert_equal "$output" "$expected"
}

@test 'run-length decode a string:lowercase string' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r '
                include "./run-length-encoding";
                if .property == "encode" then
                    .input.string | encode
                elif .property == "decode" then
                    .input.string | decode
                else
                    .input.string | encode | decode
                end
    ' << 'END_INPUT'
        {
          "property": "decode",
          "input": {
            "string": "2a3b4c"
          }
        }
END_INPUT

    assert_success
    expected='aabbbcccc'
    assert_equal "$output" "$expected"
}

@test 'encode and then decode:encode followed by decode gives original string' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r '
                include "./run-length-encoding";
                if .property == "encode" then
                    .input.string | encode
                elif .property == "decode" then
                    .input.string | decode
                else
                    .input.string | encode | decode
                end
    ' << 'END_INPUT'
        {
          "property": "consistency",
          "input": {
            "string": "zzz ZZ  zZ"
          }
        }
END_INPUT

    assert_success
    expected='zzz ZZ  zZ'
    assert_equal "$output" "$expected"
}

