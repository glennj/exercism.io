#!/usr/bin/env bats
# generated on 2026-06-28T06:41:13+00:00
load bats-extra
load bats-jq

@test 'resident who drinks water' {
    # [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f zebra-puzzle.jq << 'END_INPUT'
        {
          "property": "drinksWater"
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
          "property": "ownsZebra"
        }
END_INPUT

    assert_success
    expected='Japanese'
    assert_equal "$output" "$expected"
}
