package Lasagna;

use v5.30;
use feature 'signatures';

our $ExpectedMinutesInOven = 40;
our $PrepTimePerLayer = 2;

sub remaining_minutes_in_oven ($actual_minutes_in_oven) {
    return $ExpectedMinutesInOven - $actual_minutes_in_oven;
}

sub preparation_time_in_minutes ($number_of_layers) {
    return $PrepTimePerLayer * $number_of_layers;
}

sub total_time_in_minutes ( $number_of_layers, $actual_minutes_in_oven ) {
    return preparation_time_in_minutes($number_of_layers) + $actual_minutes_in_oven;
}

sub oven_alarm () {
    return "Ding!";
}
