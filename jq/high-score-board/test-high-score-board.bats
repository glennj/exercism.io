#!/usr/bin/env bats
load bats-extra

assert_key_value() {
    local key=$1 expected=$2 actual
    actual=$(jq -rc --arg key "$key" '.[$key]' <<< "$output")
    assert_equal "$actual" "$expected"
}

@test creates_a_new_board_with_a_test_entry {
    ## task 1
    run jq -n -c '
        include "high-score-board";
        create_score_board
    '
    assert_success
    assert_output '{"The Best Ever":1000000}'
}

@test adds_a_player_and_score_to_the_board {
    ## task 2
    run jq '
        include "high-score-board";
        add_player("Jesse Johnson"; 1337)
    ' << END_INPUT
        {
            "Amil Pastorius": 99373,
            "Min-seo Shin": 0
        }
END_INPUT
    assert_success
    assert_key_value "Amil Pastorius" 99373 "$output"
    assert_key_value "Min-seo Shin"       0 "$output"
    assert_key_value "Jesse Johnson"   1337 "$output"
}

@test removes_a_player_from_the_score_board {
    ## task 3
    run jq -c '
        include "high-score-board";
        remove_player("Jesse Johnson")
    ' << END_INPUT
        {
            "Amil Pastorius": 99373,
            "Min-seo Shin": 0,
            "Jesse Johnson": 1337
        }
END_INPUT
    assert_success
    assert_output '{"Amil Pastorius":99373,"Min-seo Shin":0}'
}

@test does_nothing_if_the_player_is_not_on_the_board {
    ## task 3
    run jq -c '
        include "high-score-board";
        remove_player("Bruno Santangelo")
    ' << END_INPUT
        {
            "Amil Pastorius": 99373,
            "Min-seo Shin": 0,
            "Jesse Johnson": 1337
        }
END_INPUT
    assert_success
    assert_output '{"Amil Pastorius":99373,"Min-seo Shin":0,"Jesse Johnson":1337}'
}

@test increases_a_players_score {
    ## task 4
    run jq '
        include "high-score-board";
        .
        | update_score("Min-seo Shin"; 1999)
        | update_score("Jesse Johnson"; 1337)
    ' << END_INPUT
        {
            "Amil Pastorius": 99373,
            "Min-seo Shin": 0,
            "Jesse Johnson": 1337
        }
END_INPUT
    assert_success
    assert_key_value "Amil Pastorius" 99373 "$output"
    assert_key_value "Min-seo Shin"    1999 "$output"
    assert_key_value "Jesse Johnson"   2674 "$output"
}

@test adds_100_points_for_all_players {
    ## task 5
    run jq '
        include "high-score-board";
        apply_monday_bonus
    ' << END_INPUT
        {
            "Amil Pastorius": 345,
            "Min-seo Shin": 19,
            "Jesse Johnson": 122
        }
END_INPUT
    assert_success
    assert_key_value "Amil Pastorius" 445 "$output"
    assert_key_value "Min-seo Shin"   119 "$output"
    assert_key_value "Jesse Johnson"  222 "$output"
}

@test does_nothing_if_the_score_board_is_empty {
    ## task 5
    run jq -c '
        include "high-score-board";
        apply_monday_bonus
    ' <<< '{}'
    assert_success
    assert_output '{}'
}

@test total_score {
    ## task 6
    run jq -c '
        include "high-score-board";
        total_score
    ' << END_INPUT
        {
            "Amil Pastorius": 345,
            "Min-seo Shin": 19,
            "Jesse Johnson": 122
        }
END_INPUT
    assert_success
    assert_output 486
}

@test total_score_empty_board {
    ## task 6
    run jq -c '
        include "high-score-board";
        total_score
    ' <<< '{}'
    assert_success
    assert_output 0
}
