#!/usr/bin/env bash
#
# added tests for 1888 and 1999

@test "1 is a single I" {
  run bash roman_numerals.sh 1
  [ "$status" -eq 0 ]
  [ "$output" == "I" ]
}

@test "2 is two I's" {
  run bash roman_numerals.sh 2
  [ "$status" -eq 0 ]
  [ "$output" == "II" ]
}

@test "3 is three I's" {
  run bash roman_numerals.sh 3
  [ "$status" -eq 0 ]
  [ "$output" == "III" ]
}

@test "4, being 5 - 1, is IV" {
  run bash roman_numerals.sh 4
  [ "$status" -eq 0 ]
  [ "$output" == "IV" ]
}

@test "5 is a single V" {
  run bash roman_numerals.sh 5
  [ "$status" -eq 0 ]
  [ "$output" == "V" ]
}

@test "6, being 5 + 1, is VI" {
  run bash roman_numerals.sh 6
  [ "$status" -eq 0 ]
  [ "$output" == "VI" ]
}

@test "9, being 10 - 1, is IX" {
  run bash roman_numerals.sh 9
  [ "$status" -eq 0 ]
  [ "$output" == "IX" ]
}

@test "20 is two X's" {
  run bash roman_numerals.sh 27
  [ "$status" -eq 0 ]
  [ "$output" == "XXVII" ]
}

@test "48 is not 50 - 2 but rather 40 + 8" {
  run bash roman_numerals.sh 48
  [ "$status" -eq 0 ]
  [ "$output" == "XLVIII" ]
}

@test "49 is not 40 + 5 + 4 but rather 50 - 10 + 10 - 1" {
  run bash roman_numerals.sh 49
  [ "$status" -eq 0 ]
  [ "$output" == "XLIX" ]
}

@test "50 is a single L" {
  run bash roman_numerals.sh 59
  [ "$status" -eq 0 ]
  [ "$output" == "LIX" ]
}

@test "90, being 100 - 10, is XC" {
  run bash roman_numerals.sh 93
  [ "$status" -eq 0 ]
  [ "$output" == "XCIII" ]
}

@test "100 is a single C" {
  run bash roman_numerals.sh 141
  [ "$status" -eq 0 ]
  [ "$output" == "CXLI" ]
}

@test "60, being 50 + 10, is LX" {
  run bash roman_numerals.sh 163
  [ "$status" -eq 0 ]
  [ "$output" == "CLXIII" ]
}

@test "400, being 500 - 100, is CD" {
  run bash roman_numerals.sh 402
  [ "$status" -eq 0 ]
  [ "$output" == "CDII" ]
}

@test "500 is a single D" {
  run bash roman_numerals.sh 575
  [ "$status" -eq 0 ]
  [ "$output" == "DLXXV" ]
}

@test "900, being 1000 - 100, is CM" {
  run bash roman_numerals.sh 911
  [ "$status" -eq 0 ]
  [ "$output" == "CMXI" ]
}

@test "1000 is a single M" {
  run bash roman_numerals.sh 1024
  [ "$status" -eq 0 ]
  [ "$output" == "MXXIV" ]
}

@test "3000 is three M's" {
  run bash roman_numerals.sh 3000
  [ "$status" -eq 0 ]
  [ "$output" == "MMM" ]
}

@test "1888" {
  run bash roman_numerals.sh 1888
  [ "$status" -eq 0 ]
  [ "$output" == "MDCCCLXXXVIII" ]
}

@test "1999" {
  run bash roman_numerals.sh 1999
  [ "$status" -eq 0 ]
  [ "$output" == "MCMXCIX" ]
}

