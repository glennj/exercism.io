#!/usr/bin/env bats
# generated on 2022-11-02T20:58:57Z
load bats-extra

@test 'encode:encode yes' {
    #[[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f atbash-cipher.jq << 'END_INPUT'
        {
          "property": "encode",
          "input": {
            "phrase": "yes"
          }
        }
END_INPUT

    assert_success
    expected='bvh'
    assert_equal "$output" "$expected"
}

@test 'encode:encode no' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f atbash-cipher.jq << 'END_INPUT'
        {
          "property": "encode",
          "input": {
            "phrase": "no"
          }
        }
END_INPUT

    assert_success
    expected='ml'
    assert_equal "$output" "$expected"
}

@test 'encode:encode OMG' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f atbash-cipher.jq << 'END_INPUT'
        {
          "property": "encode",
          "input": {
            "phrase": "OMG"
          }
        }
END_INPUT

    assert_success
    expected='lnt'
    assert_equal "$output" "$expected"
}

@test 'encode:encode spaces' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f atbash-cipher.jq << 'END_INPUT'
        {
          "property": "encode",
          "input": {
            "phrase": "O M G"
          }
        }
END_INPUT

    assert_success
    expected='lnt'
    assert_equal "$output" "$expected"
}

@test 'encode:encode mindblowingly' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f atbash-cipher.jq << 'END_INPUT'
        {
          "property": "encode",
          "input": {
            "phrase": "mindblowingly"
          }
        }
END_INPUT

    assert_success
    expected='nrmwy oldrm tob'
    assert_equal "$output" "$expected"
}

@test 'encode:encode numbers' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f atbash-cipher.jq << 'END_INPUT'
        {
          "property": "encode",
          "input": {
            "phrase": "Testing,1 2 3, testing."
          }
        }
END_INPUT

    assert_success
    expected='gvhgr mt123 gvhgr mt'
    assert_equal "$output" "$expected"
}

@test 'encode:encode deep thought' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f atbash-cipher.jq << 'END_INPUT'
        {
          "property": "encode",
          "input": {
            "phrase": "Truth is fiction."
          }
        }
END_INPUT

    assert_success
    expected='gifgs rhurx grlm'
    assert_equal "$output" "$expected"
}

@test 'encode:encode all the letters' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f atbash-cipher.jq << 'END_INPUT'
        {
          "property": "encode",
          "input": {
            "phrase": "The quick brown fox jumps over the lazy dog."
          }
        }
END_INPUT

    assert_success
    expected='gsvjf rxpyi ldmul cqfnk hlevi gsvoz abwlt'
    assert_equal "$output" "$expected"
}

@test 'decode:decode exercism' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f atbash-cipher.jq << 'END_INPUT'
        {
          "property": "decode",
          "input": {
            "phrase": "vcvix rhn"
          }
        }
END_INPUT

    assert_success
    expected='exercism'
    assert_equal "$output" "$expected"
}

@test 'decode:decode a sentence' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f atbash-cipher.jq << 'END_INPUT'
        {
          "property": "decode",
          "input": {
            "phrase": "zmlyh gzxov rhlug vmzhg vkkrm thglm v"
          }
        }
END_INPUT

    assert_success
    expected='anobstacleisoftenasteppingstone'
    assert_equal "$output" "$expected"
}

@test 'decode:decode numbers' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f atbash-cipher.jq << 'END_INPUT'
        {
          "property": "decode",
          "input": {
            "phrase": "gvhgr mt123 gvhgr mt"
          }
        }
END_INPUT

    assert_success
    expected='testing123testing'
    assert_equal "$output" "$expected"
}

@test 'decode:decode all the letters' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f atbash-cipher.jq << 'END_INPUT'
        {
          "property": "decode",
          "input": {
            "phrase": "gsvjf rxpyi ldmul cqfnk hlevi gsvoz abwlt"
          }
        }
END_INPUT

    assert_success
    expected='thequickbrownfoxjumpsoverthelazydog'
    assert_equal "$output" "$expected"
}

@test 'decode:decode with too many spaces' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f atbash-cipher.jq << 'END_INPUT'
        {
          "property": "decode",
          "input": {
            "phrase": "vc vix    r hn"
          }
        }
END_INPUT

    assert_success
    expected='exercism'
    assert_equal "$output" "$expected"
}

@test 'decode:decode with no spaces' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f atbash-cipher.jq << 'END_INPUT'
        {
          "property": "decode",
          "input": {
            "phrase": "zmlyhgzxovrhlugvmzhgvkkrmthglmv"
          }
        }
END_INPUT

    assert_success
    expected='anobstacleisoftenasteppingstone'
    assert_equal "$output" "$expected"
}

