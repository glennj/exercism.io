#!/usr/bin/env bats
# generated on 2023-11-07T18:49:21Z
load bats-extra
load bats-jq

assert_objects_equal() {
    local result=$(
        jq -n --argjson actual "$1" \
              --argjson expected "$2" \
            '$actual == $expected'
    )
    [[ $result == "true" ]]
}

@test 'empty strand' {
    #[[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f nucleotide-count.jq << 'END_INPUT'
        {
          "strand": ""
        }
END_INPUT

    assert_success
    expected='{"A":0,"C":0,"G":0,"T":0}'
    assert_objects_equal "$output" "$expected"
}

@test 'can count one nucleotide in single-character input' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f nucleotide-count.jq << 'END_INPUT'
        {
          "strand": "G"
        }
END_INPUT

    assert_success
    expected='{"A":0,"C":0,"G":1,"T":0}'
    assert_objects_equal "$output" "$expected"
}

@test 'strand with repeated nucleotide' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f nucleotide-count.jq << 'END_INPUT'
        {
          "strand": "GGGGGGG"
        }
END_INPUT

    assert_success
    expected='{"A":0,"C":0,"G":7,"T":0}'
    assert_objects_equal "$output" "$expected"
}

@test 'strand with multiple nucleotides' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f nucleotide-count.jq << 'END_INPUT'
        {
          "strand": "AGCTTTTCATTCTGACTGCAACGGGCAATATGTCTCTGTGTGGATTAAAAAAAGAGTGTCTGATAGCAGC"
        }
END_INPUT

    assert_success
    expected='{"A":20,"C":12,"G":17,"T":21}'
    assert_objects_equal "$output" "$expected"
}

@test 'strand with invalid nucleotides' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f nucleotide-count.jq << 'END_INPUT'
        {
          "strand": "AGXXACT"
        }
END_INPUT

    assert_failure
    expected='Invalid nucleotide in strand'
    assert_equal "$output" "$expected"
}
