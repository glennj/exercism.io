#!/usr/bin/env bats
load bats-extra
load bats-jq

@test 'just the header if no input' {
    #[[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f tournament.jq << 'END_INPUT'
        {
          "matches": []
        }
END_INPUT

    expected=$(cat << END_EXPECTED
Team                           | MP |  W |  D |  L |  P
END_EXPECTED
)
    assert_success
    assert_output "$expected"
}

@test 'a win is three points, a loss is zero points' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f tournament.jq << 'END_INPUT'
        {
          "matches": [
            "Allegoric Alaskans;Blithering Badgers;win"
          ]
        }
END_INPUT

    expected=$(cat << END_EXPECTED
Team                           | MP |  W |  D |  L |  P
Allegoric Alaskans             |  1 |  1 |  0 |  0 |  3
Blithering Badgers             |  1 |  0 |  0 |  1 |  0
END_EXPECTED
)
    assert_success
    assert_output "$expected"
}

@test 'a win can also be expressed as a loss' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f tournament.jq << 'END_INPUT'
        {
          "matches": [
            "Blithering Badgers;Allegoric Alaskans;loss"
          ]
        }
END_INPUT

    expected=$(cat << END_EXPECTED
Team                           | MP |  W |  D |  L |  P
Allegoric Alaskans             |  1 |  1 |  0 |  0 |  3
Blithering Badgers             |  1 |  0 |  0 |  1 |  0
END_EXPECTED
)
    assert_success
    assert_output "$expected"
}

@test 'a different team can win' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f tournament.jq << 'END_INPUT'
        {
          "matches": [
            "Blithering Badgers;Allegoric Alaskans;win"
          ]
        }
END_INPUT

    expected=$(cat << END_EXPECTED
Team                           | MP |  W |  D |  L |  P
Blithering Badgers             |  1 |  1 |  0 |  0 |  3
Allegoric Alaskans             |  1 |  0 |  0 |  1 |  0
END_EXPECTED
)
    assert_success
    assert_output "$expected"
}

@test 'a draw is one point each' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f tournament.jq << 'END_INPUT'
        {
          "matches": [
            "Allegoric Alaskans;Blithering Badgers;draw"
          ]
        }
END_INPUT

    expected=$(cat << END_EXPECTED
Team                           | MP |  W |  D |  L |  P
Allegoric Alaskans             |  1 |  0 |  1 |  0 |  1
Blithering Badgers             |  1 |  0 |  1 |  0 |  1
END_EXPECTED
)
    assert_success
    assert_output "$expected"
}

@test 'There can be more than one match' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f tournament.jq << 'END_INPUT'
        {
          "matches": [
            "Allegoric Alaskans;Blithering Badgers;win",
            "Allegoric Alaskans;Blithering Badgers;win"
          ]
        }
END_INPUT

    expected=$(cat << END_EXPECTED
Team                           | MP |  W |  D |  L |  P
Allegoric Alaskans             |  2 |  2 |  0 |  0 |  6
Blithering Badgers             |  2 |  0 |  0 |  2 |  0
END_EXPECTED
)
    assert_success
    assert_output "$expected"
}

@test 'There can be more than one winner' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f tournament.jq << 'END_INPUT'
        {
          "matches": [
            "Allegoric Alaskans;Blithering Badgers;loss",
            "Allegoric Alaskans;Blithering Badgers;win"
          ]
        }
END_INPUT

    expected=$(cat << END_EXPECTED
Team                           | MP |  W |  D |  L |  P
Allegoric Alaskans             |  2 |  1 |  0 |  1 |  3
Blithering Badgers             |  2 |  1 |  0 |  1 |  3
END_EXPECTED
)
    assert_success
    assert_output "$expected"
}

@test 'There can be more than two teams' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f tournament.jq << 'END_INPUT'
        {
          "matches": [
            "Allegoric Alaskans;Blithering Badgers;win",
            "Blithering Badgers;Courageous Californians;win",
            "Courageous Californians;Allegoric Alaskans;loss"
          ]
        }
END_INPUT

    expected=$(cat << END_EXPECTED
Team                           | MP |  W |  D |  L |  P
Allegoric Alaskans             |  2 |  2 |  0 |  0 |  6
Blithering Badgers             |  2 |  1 |  0 |  1 |  3
Courageous Californians        |  2 |  0 |  0 |  2 |  0
END_EXPECTED
)
    assert_success
    assert_output "$expected"
}

@test 'typical input' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f tournament.jq << 'END_INPUT'
        {
          "matches": [
            "Allegoric Alaskans;Blithering Badgers;win",
            "Devastating Donkeys;Courageous Californians;draw",
            "Devastating Donkeys;Allegoric Alaskans;win",
            "Courageous Californians;Blithering Badgers;loss",
            "Blithering Badgers;Devastating Donkeys;loss",
            "Allegoric Alaskans;Courageous Californians;win"
          ]
        }
END_INPUT

    expected=$(cat << END_EXPECTED
Team                           | MP |  W |  D |  L |  P
Devastating Donkeys            |  3 |  2 |  1 |  0 |  7
Allegoric Alaskans             |  3 |  2 |  0 |  1 |  6
Blithering Badgers             |  3 |  1 |  0 |  2 |  3
Courageous Californians        |  3 |  0 |  1 |  2 |  1
END_EXPECTED
)
    assert_success
    assert_output "$expected"
}

@test 'incomplete competition (not all pairs have played)' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f tournament.jq << 'END_INPUT'
        {
          "matches": [
            "Allegoric Alaskans;Blithering Badgers;loss",
            "Devastating Donkeys;Allegoric Alaskans;loss",
            "Courageous Californians;Blithering Badgers;draw",
            "Allegoric Alaskans;Courageous Californians;win"
          ]
        }
END_INPUT

    expected=$(cat << END_EXPECTED
Team                           | MP |  W |  D |  L |  P
Allegoric Alaskans             |  3 |  2 |  0 |  1 |  6
Blithering Badgers             |  2 |  1 |  1 |  0 |  4
Courageous Californians        |  2 |  0 |  1 |  1 |  1
Devastating Donkeys            |  1 |  0 |  0 |  1 |  0
END_EXPECTED
)
    assert_success
    assert_output "$expected"
}

@test 'ties broken alphabetically' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f tournament.jq << 'END_INPUT'
        {
          "matches": [
            "Courageous Californians;Devastating Donkeys;win",
            "Allegoric Alaskans;Blithering Badgers;win",
            "Devastating Donkeys;Allegoric Alaskans;loss",
            "Courageous Californians;Blithering Badgers;win",
            "Blithering Badgers;Devastating Donkeys;draw",
            "Allegoric Alaskans;Courageous Californians;draw"
          ]
        }
END_INPUT

    expected=$(cat << END_EXPECTED
Team                           | MP |  W |  D |  L |  P
Allegoric Alaskans             |  3 |  2 |  1 |  0 |  7
Courageous Californians        |  3 |  2 |  1 |  0 |  7
Blithering Badgers             |  3 |  0 |  1 |  2 |  1
Devastating Donkeys            |  3 |  0 |  1 |  2 |  1
END_EXPECTED
)
    assert_success
    assert_output "$expected"
}

@test 'ensure points sorted numerically' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f tournament.jq << 'END_INPUT'
        {
          "matches": [
            "Devastating Donkeys;Blithering Badgers;win",
            "Devastating Donkeys;Blithering Badgers;win",
            "Devastating Donkeys;Blithering Badgers;win",
            "Devastating Donkeys;Blithering Badgers;win",
            "Blithering Badgers;Devastating Donkeys;win"
          ]
        }
END_INPUT

    expected=$(cat << END_EXPECTED
Team                           | MP |  W |  D |  L |  P
Devastating Donkeys            |  5 |  4 |  0 |  1 | 12
Blithering Badgers             |  5 |  1 |  0 |  4 |  3
END_EXPECTED
)
    assert_success
    assert_output "$expected"
}
