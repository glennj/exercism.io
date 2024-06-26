#!/usr/bin/env bats
# generated on 2024-06-07T20:49:24Z
load bats-extra
load bats-jq

@test 'ay is added to words that start with vowels:word beginning with a' {
    #[[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f pig-latin.jq << 'END_INPUT'
        {
          "phrase": "apple"
        }
END_INPUT

    assert_success
    expected='appleay'
    assert_equal "$output" "$expected"
}

@test 'ay is added to words that start with vowels:word beginning with e' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f pig-latin.jq << 'END_INPUT'
        {
          "phrase": "ear"
        }
END_INPUT

    assert_success
    expected='earay'
    assert_equal "$output" "$expected"
}

@test 'ay is added to words that start with vowels:word beginning with i' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f pig-latin.jq << 'END_INPUT'
        {
          "phrase": "igloo"
        }
END_INPUT

    assert_success
    expected='iglooay'
    assert_equal "$output" "$expected"
}

@test 'ay is added to words that start with vowels:word beginning with o' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f pig-latin.jq << 'END_INPUT'
        {
          "phrase": "object"
        }
END_INPUT

    assert_success
    expected='objectay'
    assert_equal "$output" "$expected"
}

@test 'ay is added to words that start with vowels:word beginning with u' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f pig-latin.jq << 'END_INPUT'
        {
          "phrase": "under"
        }
END_INPUT

    assert_success
    expected='underay'
    assert_equal "$output" "$expected"
}

@test 'ay is added to words that start with vowels:word beginning with a vowel and followed by a qu' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f pig-latin.jq << 'END_INPUT'
        {
          "phrase": "equal"
        }
END_INPUT

    assert_success
    expected='equalay'
    assert_equal "$output" "$expected"
}

@test 'first letter and ay are moved to the end of words that start with consonants:word beginning with p' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f pig-latin.jq << 'END_INPUT'
        {
          "phrase": "pig"
        }
END_INPUT

    assert_success
    expected='igpay'
    assert_equal "$output" "$expected"
}

@test 'first letter and ay are moved to the end of words that start with consonants:word beginning with k' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f pig-latin.jq << 'END_INPUT'
        {
          "phrase": "koala"
        }
END_INPUT

    assert_success
    expected='oalakay'
    assert_equal "$output" "$expected"
}

@test 'first letter and ay are moved to the end of words that start with consonants:word beginning with x' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f pig-latin.jq << 'END_INPUT'
        {
          "phrase": "xenon"
        }
END_INPUT

    assert_success
    expected='enonxay'
    assert_equal "$output" "$expected"
}

@test 'first letter and ay are moved to the end of words that start with consonants:word beginning with q without a following u' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f pig-latin.jq << 'END_INPUT'
        {
          "phrase": "qat"
        }
END_INPUT

    assert_success
    expected='atqay'
    assert_equal "$output" "$expected"
}

@test 'some letter clusters are treated like a single consonant:word beginning with ch' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f pig-latin.jq << 'END_INPUT'
        {
          "phrase": "chair"
        }
END_INPUT

    assert_success
    expected='airchay'
    assert_equal "$output" "$expected"
}

@test 'some letter clusters are treated like a single consonant:word beginning with qu' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f pig-latin.jq << 'END_INPUT'
        {
          "phrase": "queen"
        }
END_INPUT

    assert_success
    expected='eenquay'
    assert_equal "$output" "$expected"
}

@test 'some letter clusters are treated like a single consonant:word beginning with qu and a preceding consonant' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f pig-latin.jq << 'END_INPUT'
        {
          "phrase": "square"
        }
END_INPUT

    assert_success
    expected='aresquay'
    assert_equal "$output" "$expected"
}

@test 'some letter clusters are treated like a single consonant:word beginning with th' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f pig-latin.jq << 'END_INPUT'
        {
          "phrase": "therapy"
        }
END_INPUT

    assert_success
    expected='erapythay'
    assert_equal "$output" "$expected"
}

@test 'some letter clusters are treated like a single consonant:word beginning with thr' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f pig-latin.jq << 'END_INPUT'
        {
          "phrase": "thrush"
        }
END_INPUT

    assert_success
    expected='ushthray'
    assert_equal "$output" "$expected"
}

@test 'some letter clusters are treated like a single consonant:word beginning with sch' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f pig-latin.jq << 'END_INPUT'
        {
          "phrase": "school"
        }
END_INPUT

    assert_success
    expected='oolschay'
    assert_equal "$output" "$expected"
}

@test 'some letter clusters are treated like a single vowel:word beginning with yt' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f pig-latin.jq << 'END_INPUT'
        {
          "phrase": "yttria"
        }
END_INPUT

    assert_success
    expected='yttriaay'
    assert_equal "$output" "$expected"
}

@test 'some letter clusters are treated like a single vowel:word beginning with xr' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f pig-latin.jq << 'END_INPUT'
        {
          "phrase": "xray"
        }
END_INPUT

    assert_success
    expected='xrayay'
    assert_equal "$output" "$expected"
}

@test 'position of y in a word determines if it is a consonant or a vowel:y is treated like a consonant at the beginning of a word' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f pig-latin.jq << 'END_INPUT'
        {
          "phrase": "yellow"
        }
END_INPUT

    assert_success
    expected='ellowyay'
    assert_equal "$output" "$expected"
}

@test 'position of y in a word determines if it is a consonant or a vowel:y is treated like a vowel at the end of a consonant cluster' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f pig-latin.jq << 'END_INPUT'
        {
          "phrase": "rhythm"
        }
END_INPUT

    assert_success
    expected='ythmrhay'
    assert_equal "$output" "$expected"
}

@test 'position of y in a word determines if it is a consonant or a vowel:y as second letter in two letter word' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f pig-latin.jq << 'END_INPUT'
        {
          "phrase": "my"
        }
END_INPUT

    assert_success
    expected='ymay'
    assert_equal "$output" "$expected"
}

@test 'phrases are translated:a whole phrase' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f pig-latin.jq << 'END_INPUT'
        {
          "phrase": "quick fast run"
        }
END_INPUT

    assert_success
    expected='ickquay astfay unray'
    assert_equal "$output" "$expected"
}
