#!/usr/bin/env bats
# generated on 2022-11-02T20:59:55Z
load bats-extra

@test 'resident who drinks water' {
    #[[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f zebra-puzzle.jq << 'END_INPUT'
        {
          "property": "drinksWater",
          "input": {}
        }
END_INPUT

    assert_success
    expected='Norwegian'
    assert_equal "$output" "$expected"
}

@test 'resident who owns zebra' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f zebra-puzzle.jq << 'END_INPUT'
        {
          "property": "ownsZebra",
          "input": {}
        }
END_INPUT

    assert_success
    expected='Japanese'
    assert_equal "$output" "$expected"
}

