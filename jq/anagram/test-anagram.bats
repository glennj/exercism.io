#!/usr/bin/env bats
# generated on 2022-11-02T20:58:55Z
load bats-extra

@test 'no matches' {
    #[[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f anagram.jq << 'END_INPUT'
        {
          "subject": "diaper",
          "candidates": [
            "hello",
            "world",
            "zombies",
            "pants"
          ]
        }
END_INPUT

    assert_success
    expected='[]'
    assert_equal "$output" "$expected"
}

@test 'detects two anagrams' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f anagram.jq << 'END_INPUT'
        {
          "subject": "solemn",
          "candidates": [
            "lemons",
            "cherry",
            "melons"
          ]
        }
END_INPUT

    assert_success
    expected='["lemons","melons"]'
    assert_equal "$output" "$expected"
}

@test 'does not detect anagram subsets' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f anagram.jq << 'END_INPUT'
        {
          "subject": "good",
          "candidates": [
            "dog",
            "goody"
          ]
        }
END_INPUT

    assert_success
    expected='[]'
    assert_equal "$output" "$expected"
}

@test 'detects anagram' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f anagram.jq << 'END_INPUT'
        {
          "subject": "listen",
          "candidates": [
            "enlists",
            "google",
            "inlets",
            "banana"
          ]
        }
END_INPUT

    assert_success
    expected='["inlets"]'
    assert_equal "$output" "$expected"
}

@test 'detects three anagrams' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f anagram.jq << 'END_INPUT'
        {
          "subject": "allergy",
          "candidates": [
            "gallery",
            "ballerina",
            "regally",
            "clergy",
            "largely",
            "leading"
          ]
        }
END_INPUT

    assert_success
    expected='["gallery","regally","largely"]'
    assert_equal "$output" "$expected"
}

@test 'detects multiple anagrams with different case' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f anagram.jq << 'END_INPUT'
        {
          "subject": "nose",
          "candidates": [
            "Eons",
            "ONES"
          ]
        }
END_INPUT

    assert_success
    expected='["Eons","ONES"]'
    assert_equal "$output" "$expected"
}

@test 'does not detect non-anagrams with identical checksum' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f anagram.jq << 'END_INPUT'
        {
          "subject": "mass",
          "candidates": [
            "last"
          ]
        }
END_INPUT

    assert_success
    expected='[]'
    assert_equal "$output" "$expected"
}

@test 'detects anagrams case-insensitively' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f anagram.jq << 'END_INPUT'
        {
          "subject": "Orchestra",
          "candidates": [
            "cashregister",
            "Carthorse",
            "radishes"
          ]
        }
END_INPUT

    assert_success
    expected='["Carthorse"]'
    assert_equal "$output" "$expected"
}

@test 'detects anagrams using case-insensitive subject' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f anagram.jq << 'END_INPUT'
        {
          "subject": "Orchestra",
          "candidates": [
            "cashregister",
            "carthorse",
            "radishes"
          ]
        }
END_INPUT

    assert_success
    expected='["carthorse"]'
    assert_equal "$output" "$expected"
}

@test 'detects anagrams using case-insensitive possible matches' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f anagram.jq << 'END_INPUT'
        {
          "subject": "orchestra",
          "candidates": [
            "cashregister",
            "Carthorse",
            "radishes"
          ]
        }
END_INPUT

    assert_success
    expected='["Carthorse"]'
    assert_equal "$output" "$expected"
}

@test 'does not detect an anagram if the original word is repeated' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f anagram.jq << 'END_INPUT'
        {
          "subject": "go",
          "candidates": [
            "go Go GO"
          ]
        }
END_INPUT

    assert_success
    expected='[]'
    assert_equal "$output" "$expected"
}

@test 'anagrams must use all letters exactly once' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f anagram.jq << 'END_INPUT'
        {
          "subject": "tapper",
          "candidates": [
            "patter"
          ]
        }
END_INPUT

    assert_success
    expected='[]'
    assert_equal "$output" "$expected"
}

@test 'words are not anagrams of themselves' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f anagram.jq << 'END_INPUT'
        {
          "subject": "BANANA",
          "candidates": [
            "BANANA"
          ]
        }
END_INPUT

    assert_success
    expected='[]'
    assert_equal "$output" "$expected"
}

@test 'words are not anagrams of themselves even if letter case is partially different' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f anagram.jq << 'END_INPUT'
        {
          "subject": "BANANA",
          "candidates": [
            "Banana"
          ]
        }
END_INPUT

    assert_success
    expected='[]'
    assert_equal "$output" "$expected"
}

@test 'words are not anagrams of themselves even if letter case is completely different' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f anagram.jq << 'END_INPUT'
        {
          "subject": "BANANA",
          "candidates": [
            "banana"
          ]
        }
END_INPUT

    assert_success
    expected='[]'
    assert_equal "$output" "$expected"
}

@test 'words other than themselves can be anagrams' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f anagram.jq << 'END_INPUT'
        {
          "subject": "LISTEN",
          "candidates": [
            "LISTEN",
            "Silent"
          ]
        }
END_INPUT

    assert_success
    expected='["Silent"]'
    assert_equal "$output" "$expected"
}

