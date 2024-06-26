#!/usr/bin/env bats
# generated on 2022-11-02T20:59:37Z
load bats-extra
load bats-jq

@test 'zero pieces' {
    #[[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f proverb.jq << 'END_INPUT'
        {
          "strings": []
        }
END_INPUT

    assert_success
    expected='[]'
    assert_equal "$output" "$expected"
}

@test 'one piece' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f proverb.jq << 'END_INPUT'
        {
          "strings": [
            "nail"
          ]
        }
END_INPUT

    assert_success
    expected='["And all for the want of a nail."]'
    assert_equal "$output" "$expected"
}

@test 'two pieces' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f proverb.jq << 'END_INPUT'
        {
          "strings": [
            "nail",
            "shoe"
          ]
        }
END_INPUT

    assert_success
    expected='["For want of a nail the shoe was lost.","And all for the want of a nail."]'
    assert_equal "$output" "$expected"
}

@test 'three pieces' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f proverb.jq << 'END_INPUT'
        {
          "strings": [
            "nail",
            "shoe",
            "horse"
          ]
        }
END_INPUT

    assert_success
    expected='["For want of a nail the shoe was lost.","For want of a shoe the horse was lost.","And all for the want of a nail."]'
    assert_equal "$output" "$expected"
}

@test 'full proverb' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f proverb.jq << 'END_INPUT'
        {
          "strings": [
            "nail",
            "shoe",
            "horse",
            "rider",
            "message",
            "battle",
            "kingdom"
          ]
        }
END_INPUT

    assert_success
    expected='["For want of a nail the shoe was lost.","For want of a shoe the horse was lost.","For want of a horse the rider was lost.","For want of a rider the message was lost.","For want of a message the battle was lost.","For want of a battle the kingdom was lost.","And all for the want of a nail."]'
    assert_equal "$output" "$expected"
}

@test 'four pieces modernized' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f proverb.jq << 'END_INPUT'
        {
          "strings": [
            "pin",
            "gun",
            "soldier",
            "battle"
          ]
        }
END_INPUT

    assert_success
    expected='["For want of a pin the gun was lost.","For want of a gun the soldier was lost.","For want of a soldier the battle was lost.","And all for the want of a pin."]'
    assert_equal "$output" "$expected"
}

