#!/usr/bin/env bats
# generated on 2022-11-02T20:59:12Z
load bats-extra

@test 'empty strands' {
    #[[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f hamming.jq << 'END_INPUT'
        {
          "strand1": "",
          "strand2": ""
        }
END_INPUT

    assert_success
    expected=0
    assert_equal "$output" "$expected"
}

@test 'single letter identical strands' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f hamming.jq << 'END_INPUT'
        {
          "strand1": "A",
          "strand2": "A"
        }
END_INPUT

    assert_success
    expected=0
    assert_equal "$output" "$expected"
}

@test 'single letter different strands' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f hamming.jq << 'END_INPUT'
        {
          "strand1": "G",
          "strand2": "T"
        }
END_INPUT

    assert_success
    expected=1
    assert_equal "$output" "$expected"
}

@test 'long identical strands' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f hamming.jq << 'END_INPUT'
        {
          "strand1": "GGACTGAAATCTG",
          "strand2": "GGACTGAAATCTG"
        }
END_INPUT

    assert_success
    expected=0
    assert_equal "$output" "$expected"
}

@test 'long different strands' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f hamming.jq << 'END_INPUT'
        {
          "strand1": "GGACGGATTCTG",
          "strand2": "AGGACGGATTCT"
        }
END_INPUT

    assert_success
    expected=9
    assert_equal "$output" "$expected"
}

@test 'disallow first strand longer' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f hamming.jq << 'END_INPUT'
        {
          "strand1": "AATG",
          "strand2": "AAA"
        }
END_INPUT

    assert_failure
    expected='strands must be of equal length'
    assert_equal "$output" "$expected"
}

@test 'disallow second strand longer' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f hamming.jq << 'END_INPUT'
        {
          "strand1": "ATA",
          "strand2": "AGTG"
        }
END_INPUT

    assert_failure
    expected='strands must be of equal length'
    assert_equal "$output" "$expected"
}

@test 'disallow empty first strand' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f hamming.jq << 'END_INPUT'
        {
          "strand1": "",
          "strand2": "G"
        }
END_INPUT

    assert_failure
    expected='strands must be of equal length'
    assert_equal "$output" "$expected"
}

@test 'disallow empty second strand' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f hamming.jq << 'END_INPUT'
        {
          "strand1": "G",
          "strand2": ""
        }
END_INPUT

    assert_failure
    expected='strands must be of equal length'
    assert_equal "$output" "$expected"
}

