#!/usr/bin/env bats
# generated on 2024-07-12T23:33:32Z
load bats-extra
load bats-jq

@test 'testing for eggs allergy:not allergic to anything' {
    #[[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f allergies.jq << 'END_INPUT'
        {
          "property": "allergicTo",
          "input": {
            "item": "eggs",
            "score": 0
          }
        }
END_INPUT

    assert_success
    expected=false
    assert_equal "$output" "$expected"
}

@test 'testing for eggs allergy:allergic only to eggs' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f allergies.jq << 'END_INPUT'
        {
          "property": "allergicTo",
          "input": {
            "item": "eggs",
            "score": 1
          }
        }
END_INPUT

    assert_success
    expected=true
    assert_equal "$output" "$expected"
}

@test 'testing for eggs allergy:allergic to eggs and something else' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f allergies.jq << 'END_INPUT'
        {
          "property": "allergicTo",
          "input": {
            "item": "eggs",
            "score": 3
          }
        }
END_INPUT

    assert_success
    expected=true
    assert_equal "$output" "$expected"
}

@test 'testing for eggs allergy:allergic to something, but not eggs' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f allergies.jq << 'END_INPUT'
        {
          "property": "allergicTo",
          "input": {
            "item": "eggs",
            "score": 2
          }
        }
END_INPUT

    assert_success
    expected=false
    assert_equal "$output" "$expected"
}

@test 'testing for eggs allergy:allergic to everything' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f allergies.jq << 'END_INPUT'
        {
          "property": "allergicTo",
          "input": {
            "item": "eggs",
            "score": 255
          }
        }
END_INPUT

    assert_success
    expected=true
    assert_equal "$output" "$expected"
}

@test 'testing for peanuts allergy:not allergic to anything' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f allergies.jq << 'END_INPUT'
        {
          "property": "allergicTo",
          "input": {
            "item": "peanuts",
            "score": 0
          }
        }
END_INPUT

    assert_success
    expected=false
    assert_equal "$output" "$expected"
}

@test 'testing for peanuts allergy:allergic only to peanuts' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f allergies.jq << 'END_INPUT'
        {
          "property": "allergicTo",
          "input": {
            "item": "peanuts",
            "score": 2
          }
        }
END_INPUT

    assert_success
    expected=true
    assert_equal "$output" "$expected"
}

@test 'testing for peanuts allergy:allergic to peanuts and something else' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f allergies.jq << 'END_INPUT'
        {
          "property": "allergicTo",
          "input": {
            "item": "peanuts",
            "score": 7
          }
        }
END_INPUT

    assert_success
    expected=true
    assert_equal "$output" "$expected"
}

@test 'testing for peanuts allergy:allergic to something, but not peanuts' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f allergies.jq << 'END_INPUT'
        {
          "property": "allergicTo",
          "input": {
            "item": "peanuts",
            "score": 5
          }
        }
END_INPUT

    assert_success
    expected=false
    assert_equal "$output" "$expected"
}

@test 'testing for peanuts allergy:allergic to everything' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f allergies.jq << 'END_INPUT'
        {
          "property": "allergicTo",
          "input": {
            "item": "peanuts",
            "score": 255
          }
        }
END_INPUT

    assert_success
    expected=true
    assert_equal "$output" "$expected"
}

@test 'testing for shellfish allergy:not allergic to anything' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f allergies.jq << 'END_INPUT'
        {
          "property": "allergicTo",
          "input": {
            "item": "shellfish",
            "score": 0
          }
        }
END_INPUT

    assert_success
    expected=false
    assert_equal "$output" "$expected"
}

@test 'testing for shellfish allergy:allergic only to shellfish' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f allergies.jq << 'END_INPUT'
        {
          "property": "allergicTo",
          "input": {
            "item": "shellfish",
            "score": 4
          }
        }
END_INPUT

    assert_success
    expected=true
    assert_equal "$output" "$expected"
}

@test 'testing for shellfish allergy:allergic to shellfish and something else' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f allergies.jq << 'END_INPUT'
        {
          "property": "allergicTo",
          "input": {
            "item": "shellfish",
            "score": 14
          }
        }
END_INPUT

    assert_success
    expected=true
    assert_equal "$output" "$expected"
}

@test 'testing for shellfish allergy:allergic to something, but not shellfish' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f allergies.jq << 'END_INPUT'
        {
          "property": "allergicTo",
          "input": {
            "item": "shellfish",
            "score": 10
          }
        }
END_INPUT

    assert_success
    expected=false
    assert_equal "$output" "$expected"
}

@test 'testing for shellfish allergy:allergic to everything' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f allergies.jq << 'END_INPUT'
        {
          "property": "allergicTo",
          "input": {
            "item": "shellfish",
            "score": 255
          }
        }
END_INPUT

    assert_success
    expected=true
    assert_equal "$output" "$expected"
}

@test 'testing for strawberries allergy:not allergic to anything' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f allergies.jq << 'END_INPUT'
        {
          "property": "allergicTo",
          "input": {
            "item": "strawberries",
            "score": 0
          }
        }
END_INPUT

    assert_success
    expected=false
    assert_equal "$output" "$expected"
}

@test 'testing for strawberries allergy:allergic only to strawberries' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f allergies.jq << 'END_INPUT'
        {
          "property": "allergicTo",
          "input": {
            "item": "strawberries",
            "score": 8
          }
        }
END_INPUT

    assert_success
    expected=true
    assert_equal "$output" "$expected"
}

@test 'testing for strawberries allergy:allergic to strawberries and something else' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f allergies.jq << 'END_INPUT'
        {
          "property": "allergicTo",
          "input": {
            "item": "strawberries",
            "score": 28
          }
        }
END_INPUT

    assert_success
    expected=true
    assert_equal "$output" "$expected"
}

@test 'testing for strawberries allergy:allergic to something, but not strawberries' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f allergies.jq << 'END_INPUT'
        {
          "property": "allergicTo",
          "input": {
            "item": "strawberries",
            "score": 20
          }
        }
END_INPUT

    assert_success
    expected=false
    assert_equal "$output" "$expected"
}

@test 'testing for strawberries allergy:allergic to everything' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f allergies.jq << 'END_INPUT'
        {
          "property": "allergicTo",
          "input": {
            "item": "strawberries",
            "score": 255
          }
        }
END_INPUT

    assert_success
    expected=true
    assert_equal "$output" "$expected"
}

@test 'testing for tomatoes allergy:not allergic to anything' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f allergies.jq << 'END_INPUT'
        {
          "property": "allergicTo",
          "input": {
            "item": "tomatoes",
            "score": 0
          }
        }
END_INPUT

    assert_success
    expected=false
    assert_equal "$output" "$expected"
}

@test 'testing for tomatoes allergy:allergic only to tomatoes' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f allergies.jq << 'END_INPUT'
        {
          "property": "allergicTo",
          "input": {
            "item": "tomatoes",
            "score": 16
          }
        }
END_INPUT

    assert_success
    expected=true
    assert_equal "$output" "$expected"
}

@test 'testing for tomatoes allergy:allergic to tomatoes and something else' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f allergies.jq << 'END_INPUT'
        {
          "property": "allergicTo",
          "input": {
            "item": "tomatoes",
            "score": 56
          }
        }
END_INPUT

    assert_success
    expected=true
    assert_equal "$output" "$expected"
}

@test 'testing for tomatoes allergy:allergic to something, but not tomatoes' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f allergies.jq << 'END_INPUT'
        {
          "property": "allergicTo",
          "input": {
            "item": "tomatoes",
            "score": 40
          }
        }
END_INPUT

    assert_success
    expected=false
    assert_equal "$output" "$expected"
}

@test 'testing for tomatoes allergy:allergic to everything' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f allergies.jq << 'END_INPUT'
        {
          "property": "allergicTo",
          "input": {
            "item": "tomatoes",
            "score": 255
          }
        }
END_INPUT

    assert_success
    expected=true
    assert_equal "$output" "$expected"
}

@test 'testing for chocolate allergy:not allergic to anything' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f allergies.jq << 'END_INPUT'
        {
          "property": "allergicTo",
          "input": {
            "item": "chocolate",
            "score": 0
          }
        }
END_INPUT

    assert_success
    expected=false
    assert_equal "$output" "$expected"
}

@test 'testing for chocolate allergy:allergic only to chocolate' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f allergies.jq << 'END_INPUT'
        {
          "property": "allergicTo",
          "input": {
            "item": "chocolate",
            "score": 32
          }
        }
END_INPUT

    assert_success
    expected=true
    assert_equal "$output" "$expected"
}

@test 'testing for chocolate allergy:allergic to chocolate and something else' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f allergies.jq << 'END_INPUT'
        {
          "property": "allergicTo",
          "input": {
            "item": "chocolate",
            "score": 112
          }
        }
END_INPUT

    assert_success
    expected=true
    assert_equal "$output" "$expected"
}

@test 'testing for chocolate allergy:allergic to something, but not chocolate' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f allergies.jq << 'END_INPUT'
        {
          "property": "allergicTo",
          "input": {
            "item": "chocolate",
            "score": 80
          }
        }
END_INPUT

    assert_success
    expected=false
    assert_equal "$output" "$expected"
}

@test 'testing for chocolate allergy:allergic to everything' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f allergies.jq << 'END_INPUT'
        {
          "property": "allergicTo",
          "input": {
            "item": "chocolate",
            "score": 255
          }
        }
END_INPUT

    assert_success
    expected=true
    assert_equal "$output" "$expected"
}

@test 'testing for pollen allergy:not allergic to anything' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f allergies.jq << 'END_INPUT'
        {
          "property": "allergicTo",
          "input": {
            "item": "pollen",
            "score": 0
          }
        }
END_INPUT

    assert_success
    expected=false
    assert_equal "$output" "$expected"
}

@test 'testing for pollen allergy:allergic only to pollen' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f allergies.jq << 'END_INPUT'
        {
          "property": "allergicTo",
          "input": {
            "item": "pollen",
            "score": 64
          }
        }
END_INPUT

    assert_success
    expected=true
    assert_equal "$output" "$expected"
}

@test 'testing for pollen allergy:allergic to pollen and something else' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f allergies.jq << 'END_INPUT'
        {
          "property": "allergicTo",
          "input": {
            "item": "pollen",
            "score": 224
          }
        }
END_INPUT

    assert_success
    expected=true
    assert_equal "$output" "$expected"
}

@test 'testing for pollen allergy:allergic to something, but not pollen' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f allergies.jq << 'END_INPUT'
        {
          "property": "allergicTo",
          "input": {
            "item": "pollen",
            "score": 160
          }
        }
END_INPUT

    assert_success
    expected=false
    assert_equal "$output" "$expected"
}

@test 'testing for pollen allergy:allergic to everything' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f allergies.jq << 'END_INPUT'
        {
          "property": "allergicTo",
          "input": {
            "item": "pollen",
            "score": 255
          }
        }
END_INPUT

    assert_success
    expected=true
    assert_equal "$output" "$expected"
}

@test 'testing for cats allergy:not allergic to anything' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f allergies.jq << 'END_INPUT'
        {
          "property": "allergicTo",
          "input": {
            "item": "cats",
            "score": 0
          }
        }
END_INPUT

    assert_success
    expected=false
    assert_equal "$output" "$expected"
}

@test 'testing for cats allergy:allergic only to cats' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f allergies.jq << 'END_INPUT'
        {
          "property": "allergicTo",
          "input": {
            "item": "cats",
            "score": 128
          }
        }
END_INPUT

    assert_success
    expected=true
    assert_equal "$output" "$expected"
}

@test 'testing for cats allergy:allergic to cats and something else' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f allergies.jq << 'END_INPUT'
        {
          "property": "allergicTo",
          "input": {
            "item": "cats",
            "score": 192
          }
        }
END_INPUT

    assert_success
    expected=true
    assert_equal "$output" "$expected"
}

@test 'testing for cats allergy:allergic to something, but not cats' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f allergies.jq << 'END_INPUT'
        {
          "property": "allergicTo",
          "input": {
            "item": "cats",
            "score": 64
          }
        }
END_INPUT

    assert_success
    expected=false
    assert_equal "$output" "$expected"
}

@test 'testing for cats allergy:allergic to everything' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f allergies.jq << 'END_INPUT'
        {
          "property": "allergicTo",
          "input": {
            "item": "cats",
            "score": 255
          }
        }
END_INPUT

    assert_success
    expected=true
    assert_equal "$output" "$expected"
}

@test 'list when::no allergies' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f allergies.jq << 'END_INPUT'
        {
          "property": "list",
          "input": {
            "score": 0
          }
        }
END_INPUT

    assert_success
    expected='[]'
    assert_equal "$output" "$expected"
}

@test 'list when::just eggs' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f allergies.jq << 'END_INPUT'
        {
          "property": "list",
          "input": {
            "score": 1
          }
        }
END_INPUT

    assert_success
    expected='["eggs"]'
    assert_equal "$output" "$expected"
}

@test 'list when::just peanuts' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f allergies.jq << 'END_INPUT'
        {
          "property": "list",
          "input": {
            "score": 2
          }
        }
END_INPUT

    assert_success
    expected='["peanuts"]'
    assert_equal "$output" "$expected"
}

@test 'list when::just strawberries' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f allergies.jq << 'END_INPUT'
        {
          "property": "list",
          "input": {
            "score": 8
          }
        }
END_INPUT

    assert_success
    expected='["strawberries"]'
    assert_equal "$output" "$expected"
}

@test 'list when::eggs and peanuts' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f allergies.jq << 'END_INPUT'
        {
          "property": "list",
          "input": {
            "score": 3
          }
        }
END_INPUT

    assert_success
    expected='["eggs","peanuts"]'
    assert_equal "$output" "$expected"
}

@test 'list when::more than eggs but not peanuts' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f allergies.jq << 'END_INPUT'
        {
          "property": "list",
          "input": {
            "score": 5
          }
        }
END_INPUT

    assert_success
    expected='["eggs","shellfish"]'
    assert_equal "$output" "$expected"
}

@test 'list when::lots of stuff' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f allergies.jq << 'END_INPUT'
        {
          "property": "list",
          "input": {
            "score": 248
          }
        }
END_INPUT

    assert_success
    expected='["strawberries","tomatoes","chocolate","pollen","cats"]'
    assert_equal "$output" "$expected"
}

@test 'list when::everything' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f allergies.jq << 'END_INPUT'
        {
          "property": "list",
          "input": {
            "score": 255
          }
        }
END_INPUT

    assert_success
    expected='["eggs","peanuts","shellfish","strawberries","tomatoes","chocolate","pollen","cats"]'
    assert_equal "$output" "$expected"
}

@test 'list when::no allergen score parts' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f allergies.jq << 'END_INPUT'
        {
          "property": "list",
          "input": {
            "score": 509
          }
        }
END_INPUT

    assert_success
    expected='["eggs","shellfish","strawberries","tomatoes","chocolate","pollen","cats"]'
    assert_equal "$output" "$expected"
}

@test 'list when::no allergen score parts without highest valid score' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f allergies.jq << 'END_INPUT'
        {
          "property": "list",
          "input": {
            "score": 257
          }
        }
END_INPUT

    assert_success
    expected='["eggs"]'
    assert_equal "$output" "$expected"
}
