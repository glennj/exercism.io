#!/usr/bin/env bats
load bats-extra

@test requires_a_license_for_a_car {
    ## task 1
    run jq -R 'include "vehicle-purchase"; needs_license' <<< 'car'
    assert_success
    assert_output "true"
}

@test requires_a_license_for_a_truck {
    ## task 1
    run jq -R 'include "vehicle-purchase"; needs_license' <<< 'truck'
    assert_success
    assert_output "true"
}

@test does_not_require_a_license_for_a_bike {
    ## task 1
    run jq -R 'include "vehicle-purchase"; needs_license' <<< 'bike'
    assert_success
    assert_output "false"
}

@test does_not_require_a_license_for_a_stroller {
    ## task 1
    run jq -R 'include "vehicle-purchase"; needs_license' <<< 'stroller'
    assert_success
    assert_output "false"
}

@test does_not_require_a_license_for_an_e-scooter {
    ## task 1
    run jq -R 'include "vehicle-purchase"; needs_license' <<< 'e-scooter'
    assert_success
    assert_output "false"
}


@test correctly_recommends_the_first_option {
    ## task 2
    run jq -r 'include "vehicle-purchase"; choose_vehicle' << END_INPUT
        ["Bugatti Veyron", "Ford Pinto"]
        ["Chery EQ", "Kia Niro Elektro"]
END_INPUT
    assert_success
    assert_line --index 0 'Bugatti Veyron is clearly the better choice.'
    assert_line --index 1 'Chery EQ is clearly the better choice.'
}

@test correctly_recommends_the_second_option {
    ## task 2
    run jq -r 'include "vehicle-purchase"; choose_vehicle' << END_INPUT
        ["Ford Pinto", "Bugatti Veyron"]
        ["2020 Gazelle Medeo", "2018 Bergamont City"]
END_INPUT
    assert_success
    assert_line --index 0 'Bugatti Veyron is clearly the better choice.'
    assert_line --index 1 '2018 Bergamont City is clearly the better choice.'
}

@test price_is_reduced_to_80%_for_age_below_3 {
    ## task 3
    run jq 'include "vehicle-purchase"; resell_price' << END_INPUT
        {"original_price": 40000, "age": 2}
        {"original_price": 40000, "age": 2.5}
END_INPUT
    assert_success
    assert_line --index 0 '32000'
    assert_line --index 1 '32000'
}

@test price_is_reduced_to_50%_for_age_above_10 {
    ## task 3
    run jq 'include "vehicle-purchase"; resell_price' << END_INPUT
        {"original_price": 40000, "age": 12}
END_INPUT
    assert_success
    assert_output '20000'
}

@test price_is_reduced_to_70%_for_between_3_and_10 {
    ## task 3
    run jq 'include "vehicle-purchase"; resell_price' << END_INPUT
        {"original_price": 25000, "age": 7}
END_INPUT
    assert_success
    assert_output '17500'
}

@test works_correctly_for_threshold_age_3 {
    ## task 3
    run jq 'include "vehicle-purchase"; resell_price' << END_INPUT
        {"original_price": 40000, "age": 3}
END_INPUT
    assert_success
    assert_output '28000'
}

@test works_correctly_for_threshold_age_10 {
    ## task 3
    run jq 'include "vehicle-purchase"; resell_price' << END_INPUT
        {"original_price": 25000, "age": 10}
END_INPUT
    assert_success
    assert_output '17500'
}
