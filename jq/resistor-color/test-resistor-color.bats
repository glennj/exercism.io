#!/usr/bin/env bats
# generated on 2022-11-02T20:59:40Z
load bats-extra

@test 'Color codes:Black' {
    #[[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f resistor-color.jq << 'END_INPUT'
        {
          "property": "colorCode",
          "input": {
            "color": "black"
          }
        }
END_INPUT

    assert_success
    expected=0
    assert_equal "$output" "$expected"
}

@test 'Color codes:White' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f resistor-color.jq << 'END_INPUT'
        {
          "property": "colorCode",
          "input": {
            "color": "white"
          }
        }
END_INPUT

    assert_success
    expected=9
    assert_equal "$output" "$expected"
}

@test 'Color codes:Orange' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f resistor-color.jq << 'END_INPUT'
        {
          "property": "colorCode",
          "input": {
            "color": "orange"
          }
        }
END_INPUT

    assert_success
    expected=3
    assert_equal "$output" "$expected"
}

@test 'Colors' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f resistor-color.jq << 'END_INPUT'
        {
          "property": "colors",
          "input": {}
        }
END_INPUT

    assert_success
    expected='["black","brown","red","orange","yellow","green","blue","violet","grey","white"]'
    assert_equal "$output" "$expected"
}

