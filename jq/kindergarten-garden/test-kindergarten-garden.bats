#!/usr/bin/env bats
# generated on 2024-07-23T22:14:38Z
load bats-extra
load bats-jq

@test 'partial garden:garden with single student' {
    #[[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f kindergarten-garden.jq << 'END_INPUT'
        {
          "diagram": "RC\nGG",
          "student": "Alice"
        }
END_INPUT

    assert_success
    expected='["radishes","clover","grass","grass"]'
    assert_equal "$output" "$expected"
}

@test 'partial garden:different garden with single student' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f kindergarten-garden.jq << 'END_INPUT'
        {
          "diagram": "VC\nRC",
          "student": "Alice"
        }
END_INPUT

    assert_success
    expected='["violets","clover","radishes","clover"]'
    assert_equal "$output" "$expected"
}

@test 'partial garden:garden with two students' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f kindergarten-garden.jq << 'END_INPUT'
        {
          "diagram": "VVCG\nVVRC",
          "student": "Bob"
        }
END_INPUT

    assert_success
    expected='["clover","grass","radishes","clover"]'
    assert_equal "$output" "$expected"
}

@test 'partial garden:multiple students for the same garden with three students:second student'\''s garden' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f kindergarten-garden.jq << 'END_INPUT'
        {
          "diagram": "VVCCGG\nVVCCGG",
          "student": "Bob"
        }
END_INPUT

    assert_success
    expected='["clover","clover","clover","clover"]'
    assert_equal "$output" "$expected"
}

@test 'partial garden:multiple students for the same garden with three students:third student'\''s garden' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f kindergarten-garden.jq << 'END_INPUT'
        {
          "diagram": "VVCCGG\nVVCCGG",
          "student": "Charlie"
        }
END_INPUT

    assert_success
    expected='["grass","grass","grass","grass"]'
    assert_equal "$output" "$expected"
}

@test 'full garden:for Alice, first student'\''s garden' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f kindergarten-garden.jq << 'END_INPUT'
        {
          "diagram": "VRCGVVRVCGGCCGVRGCVCGCGV\nVRCCCGCRRGVCGCRVVCVGCGCV",
          "student": "Alice"
        }
END_INPUT

    assert_success
    expected='["violets","radishes","violets","radishes"]'
    assert_equal "$output" "$expected"
}

@test 'full garden:for Bob, second student'\''s garden' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f kindergarten-garden.jq << 'END_INPUT'
        {
          "diagram": "VRCGVVRVCGGCCGVRGCVCGCGV\nVRCCCGCRRGVCGCRVVCVGCGCV",
          "student": "Bob"
        }
END_INPUT

    assert_success
    expected='["clover","grass","clover","clover"]'
    assert_equal "$output" "$expected"
}

@test 'full garden:for Charlie' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f kindergarten-garden.jq << 'END_INPUT'
        {
          "diagram": "VRCGVVRVCGGCCGVRGCVCGCGV\nVRCCCGCRRGVCGCRVVCVGCGCV",
          "student": "Charlie"
        }
END_INPUT

    assert_success
    expected='["violets","violets","clover","grass"]'
    assert_equal "$output" "$expected"
}

@test 'full garden:for David' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f kindergarten-garden.jq << 'END_INPUT'
        {
          "diagram": "VRCGVVRVCGGCCGVRGCVCGCGV\nVRCCCGCRRGVCGCRVVCVGCGCV",
          "student": "David"
        }
END_INPUT

    assert_success
    expected='["radishes","violets","clover","radishes"]'
    assert_equal "$output" "$expected"
}

@test 'full garden:for Eve' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f kindergarten-garden.jq << 'END_INPUT'
        {
          "diagram": "VRCGVVRVCGGCCGVRGCVCGCGV\nVRCCCGCRRGVCGCRVVCVGCGCV",
          "student": "Eve"
        }
END_INPUT

    assert_success
    expected='["clover","grass","radishes","grass"]'
    assert_equal "$output" "$expected"
}

@test 'full garden:for Fred' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f kindergarten-garden.jq << 'END_INPUT'
        {
          "diagram": "VRCGVVRVCGGCCGVRGCVCGCGV\nVRCCCGCRRGVCGCRVVCVGCGCV",
          "student": "Fred"
        }
END_INPUT

    assert_success
    expected='["grass","clover","violets","clover"]'
    assert_equal "$output" "$expected"
}

@test 'full garden:for Ginny' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f kindergarten-garden.jq << 'END_INPUT'
        {
          "diagram": "VRCGVVRVCGGCCGVRGCVCGCGV\nVRCCCGCRRGVCGCRVVCVGCGCV",
          "student": "Ginny"
        }
END_INPUT

    assert_success
    expected='["clover","grass","grass","clover"]'
    assert_equal "$output" "$expected"
}

@test 'full garden:for Harriet' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f kindergarten-garden.jq << 'END_INPUT'
        {
          "diagram": "VRCGVVRVCGGCCGVRGCVCGCGV\nVRCCCGCRRGVCGCRVVCVGCGCV",
          "student": "Harriet"
        }
END_INPUT

    assert_success
    expected='["violets","radishes","radishes","violets"]'
    assert_equal "$output" "$expected"
}

@test 'full garden:for Ileana' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f kindergarten-garden.jq << 'END_INPUT'
        {
          "diagram": "VRCGVVRVCGGCCGVRGCVCGCGV\nVRCCCGCRRGVCGCRVVCVGCGCV",
          "student": "Ileana"
        }
END_INPUT

    assert_success
    expected='["grass","clover","violets","clover"]'
    assert_equal "$output" "$expected"
}

@test 'full garden:for Joseph' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f kindergarten-garden.jq << 'END_INPUT'
        {
          "diagram": "VRCGVVRVCGGCCGVRGCVCGCGV\nVRCCCGCRRGVCGCRVVCVGCGCV",
          "student": "Joseph"
        }
END_INPUT

    assert_success
    expected='["violets","clover","violets","grass"]'
    assert_equal "$output" "$expected"
}

@test 'full garden:for Kincaid, second to last student'\''s garden' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f kindergarten-garden.jq << 'END_INPUT'
        {
          "diagram": "VRCGVVRVCGGCCGVRGCVCGCGV\nVRCCCGCRRGVCGCRVVCVGCGCV",
          "student": "Kincaid"
        }
END_INPUT

    assert_success
    expected='["grass","clover","clover","grass"]'
    assert_equal "$output" "$expected"
}

@test 'full garden:for Larry, last student'\''s garden' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -c -f kindergarten-garden.jq << 'END_INPUT'
        {
          "diagram": "VRCGVVRVCGGCCGVRGCVCGCGV\nVRCCCGCRRGVCGCRVVCVGCGCV",
          "student": "Larry"
        }
END_INPUT

    assert_success
    expected='["grass","violets","clover","violets"]'
    assert_equal "$output" "$expected"
}
