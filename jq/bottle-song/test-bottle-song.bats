#!/usr/bin/env bats
# generated on 2025-06-26T16:40:49Z
load bats-extra
load bats-jq

@test 'verse:single verse:first generic verse' {
    #[[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f bottle-song.jq << 'END_INPUT'
        {
          "startBottles": 10,
          "takeDown": 1
        }
END_INPUT

    assert_success
    expected='["Ten green bottles hanging on the wall,","Ten green bottles hanging on the wall,","And if one green bottle should accidentally fall,","There'\''ll be nine green bottles hanging on the wall."]'
    assert_equal "$output" "$expected"
}

@test 'verse:single verse:last generic verse' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f bottle-song.jq << 'END_INPUT'
        {
          "startBottles": 3,
          "takeDown": 1
        }
END_INPUT

    assert_success
    expected='["Three green bottles hanging on the wall,","Three green bottles hanging on the wall,","And if one green bottle should accidentally fall,","There'\''ll be two green bottles hanging on the wall."]'
    assert_equal "$output" "$expected"
}

@test 'verse:single verse:verse with 2 bottles' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f bottle-song.jq << 'END_INPUT'
        {
          "startBottles": 2,
          "takeDown": 1
        }
END_INPUT

    assert_success
    expected='["Two green bottles hanging on the wall,","Two green bottles hanging on the wall,","And if one green bottle should accidentally fall,","There'\''ll be one green bottle hanging on the wall."]'
    assert_equal "$output" "$expected"
}

@test 'verse:single verse:verse with 1 bottle' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f bottle-song.jq << 'END_INPUT'
        {
          "startBottles": 1,
          "takeDown": 1
        }
END_INPUT

    assert_success
    expected='["One green bottle hanging on the wall,","One green bottle hanging on the wall,","And if one green bottle should accidentally fall,","There'\''ll be no green bottles hanging on the wall."]'
    assert_equal "$output" "$expected"
}

@test 'lyrics:multiple verses:first two verses' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f bottle-song.jq << 'END_INPUT'
        {
          "startBottles": 10,
          "takeDown": 2
        }
END_INPUT

    assert_success
    expected='["Ten green bottles hanging on the wall,","Ten green bottles hanging on the wall,","And if one green bottle should accidentally fall,","There'\''ll be nine green bottles hanging on the wall.","","Nine green bottles hanging on the wall,","Nine green bottles hanging on the wall,","And if one green bottle should accidentally fall,","There'\''ll be eight green bottles hanging on the wall."]'
    assert_equal "$output" "$expected"
}

@test 'lyrics:multiple verses:last three verses' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f bottle-song.jq << 'END_INPUT'
        {
          "startBottles": 3,
          "takeDown": 3
        }
END_INPUT

    assert_success
    expected='["Three green bottles hanging on the wall,","Three green bottles hanging on the wall,","And if one green bottle should accidentally fall,","There'\''ll be two green bottles hanging on the wall.","","Two green bottles hanging on the wall,","Two green bottles hanging on the wall,","And if one green bottle should accidentally fall,","There'\''ll be one green bottle hanging on the wall.","","One green bottle hanging on the wall,","One green bottle hanging on the wall,","And if one green bottle should accidentally fall,","There'\''ll be no green bottles hanging on the wall."]'
    assert_equal "$output" "$expected"
}

@test 'lyrics:multiple verses:all verses' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f bottle-song.jq << 'END_INPUT'
        {
          "startBottles": 10,
          "takeDown": 10
        }
END_INPUT

    assert_success
    expected='["Ten green bottles hanging on the wall,","Ten green bottles hanging on the wall,","And if one green bottle should accidentally fall,","There'\''ll be nine green bottles hanging on the wall.","","Nine green bottles hanging on the wall,","Nine green bottles hanging on the wall,","And if one green bottle should accidentally fall,","There'\''ll be eight green bottles hanging on the wall.","","Eight green bottles hanging on the wall,","Eight green bottles hanging on the wall,","And if one green bottle should accidentally fall,","There'\''ll be seven green bottles hanging on the wall.","","Seven green bottles hanging on the wall,","Seven green bottles hanging on the wall,","And if one green bottle should accidentally fall,","There'\''ll be six green bottles hanging on the wall.","","Six green bottles hanging on the wall,","Six green bottles hanging on the wall,","And if one green bottle should accidentally fall,","There'\''ll be five green bottles hanging on the wall.","","Five green bottles hanging on the wall,","Five green bottles hanging on the wall,","And if one green bottle should accidentally fall,","There'\''ll be four green bottles hanging on the wall.","","Four green bottles hanging on the wall,","Four green bottles hanging on the wall,","And if one green bottle should accidentally fall,","There'\''ll be three green bottles hanging on the wall.","","Three green bottles hanging on the wall,","Three green bottles hanging on the wall,","And if one green bottle should accidentally fall,","There'\''ll be two green bottles hanging on the wall.","","Two green bottles hanging on the wall,","Two green bottles hanging on the wall,","And if one green bottle should accidentally fall,","There'\''ll be one green bottle hanging on the wall.","","One green bottle hanging on the wall,","One green bottle hanging on the wall,","And if one green bottle should accidentally fall,","There'\''ll be no green bottles hanging on the wall."]'
    assert_equal "$output" "$expected"
}
