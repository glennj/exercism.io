#!/usr/bin/env bats
# generated on 2022-11-02T20:59:43Z
load bats-extra

@test 'Empty RNA sequence' {
    #[[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r 'include "./rna-transcription"; .dna | toRna' << 'END_INPUT'
        {
          "dna": ""
        }
END_INPUT

    assert_success
    expected=''
    assert_equal "$output" "$expected"
}

@test 'RNA complement of cytosine is guanine' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r 'include "./rna-transcription"; .dna | toRna' << 'END_INPUT'
        {
          "dna": "C"
        }
END_INPUT

    assert_success
    expected='G'
    assert_equal "$output" "$expected"
}

@test 'RNA complement of guanine is cytosine' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r 'include "./rna-transcription"; .dna | toRna' << 'END_INPUT'
        {
          "dna": "G"
        }
END_INPUT

    assert_success
    expected='C'
    assert_equal "$output" "$expected"
}

@test 'RNA complement of thymine is adenine' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r 'include "./rna-transcription"; .dna | toRna' << 'END_INPUT'
        {
          "dna": "T"
        }
END_INPUT

    assert_success
    expected='A'
    assert_equal "$output" "$expected"
}

@test 'RNA complement of adenine is uracil' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r 'include "./rna-transcription"; .dna | toRna' << 'END_INPUT'
        {
          "dna": "A"
        }
END_INPUT

    assert_success
    expected='U'
    assert_equal "$output" "$expected"
}

@test 'RNA complement' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r 'include "./rna-transcription"; .dna | toRna' << 'END_INPUT'
        {
          "dna": "ACGTGGTCTTAA"
        }
END_INPUT

    assert_success
    expected='UGCACCAGAAUU'
    assert_equal "$output" "$expected"
}

