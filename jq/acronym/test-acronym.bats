#!/usr/bin/env bats
# generated on 2024-02-19T04:45:52Z
load bats-extra
load bats-jq

@test 'basic' {
    #[[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f acronym.jq << 'END_INPUT'
        {
          "phrase": "Portable Network Graphics"
        }
END_INPUT

    assert_success
    expected='PNG'
    assert_equal "$output" "$expected"
}

@test 'lowercase words' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f acronym.jq << 'END_INPUT'
        {
          "phrase": "Ruby on Rails"
        }
END_INPUT

    assert_success
    expected='ROR'
    assert_equal "$output" "$expected"
}

@test 'punctuation' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f acronym.jq << 'END_INPUT'
        {
          "phrase": "First In, First Out"
        }
END_INPUT

    assert_success
    expected='FIFO'
    assert_equal "$output" "$expected"
}

@test 'all caps word' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f acronym.jq << 'END_INPUT'
        {
          "phrase": "GNU Image Manipulation Program"
        }
END_INPUT

    assert_success
    expected='GIMP'
    assert_equal "$output" "$expected"
}

@test 'punctuation without whitespace' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f acronym.jq << 'END_INPUT'
        {
          "phrase": "Complementary metal-oxide semiconductor"
        }
END_INPUT

    assert_success
    expected='CMOS'
    assert_equal "$output" "$expected"
}

@test 'very long abbreviation' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f acronym.jq << 'END_INPUT'
        {
          "phrase": "Rolling On The Floor Laughing So Hard That My Dogs Came Over And Licked Me"
        }
END_INPUT

    assert_success
    expected='ROTFLSHTMDCOALM'
    assert_equal "$output" "$expected"
}

@test 'consecutive delimiters' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f acronym.jq << 'END_INPUT'
        {
          "phrase": "Something - I made up from thin air"
        }
END_INPUT

    assert_success
    expected='SIMUFTA'
    assert_equal "$output" "$expected"
}

@test 'apostrophes' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f acronym.jq << 'END_INPUT'
        {
          "phrase": "Halley's Comet"
        }
END_INPUT

    assert_success
    expected='HC'
    assert_equal "$output" "$expected"
}

@test 'underscore emphasis' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f acronym.jq << 'END_INPUT'
        {
          "phrase": "The Road _Not_ Taken"
        }
END_INPUT

    assert_success
    expected='TRNT'
    assert_equal "$output" "$expected"
}
