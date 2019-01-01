#!/usr/bin/env bash

# Check if the given string is an isogram

@test 'empty string' {
  run bash isogram.sh ''
  [ "$status" -eq 0 ]
  [ "$output" == 'true' ]
}

@test 'isogram with only lower case characters' {
  run bash isogram.sh 'isogram'
  [ "$status" -eq 0 ]
  [ "$output" == 'true' ]
}

@test 'word with one duplicated character' {
  run bash isogram.sh 'eleven'
  [ "$status" -eq 0 ]
  [ "$output" == 'false' ]
}

@test 'word with one duplicated character from the end of the alphabet' {
  run bash isogram.sh 'zzyzx'
  [ "$status" -eq 0 ]
  [ "$output" == 'false' ]
}

@test 'longest reported english isogram' {
  run bash isogram.sh 'subdermatoglyphic'
  [ "$status" -eq 0 ]
  [ "$output" == 'true' ]
}

@test 'word with duplicated character in mixed case' {
  run bash isogram.sh 'Alphabet'
  [ "$status" -eq 0 ]
  [ "$output" == 'false' ]
}

@test 'hypothetical isogrammic word with hyphen' {
  run bash isogram.sh 'thumbscrew-japingly'
  [ "$status" -eq 0 ]
  [ "$output" == 'true' ]
}

@test 'isogram with duplicated hyphen' {
  run bash isogram.sh 'six-year-old'
  [ "$status" -eq 0 ]
  [ "$output" == 'true' ]
}

@test 'made-up name that is an isogram' {
  run bash isogram.sh 'Emily Jung Schwartzkopf'
  [ "$status" -eq 0 ]
  [ "$output" == 'true' ]
}

@test 'duplicated character in the middle' {
  run bash isogram.sh 'accentor'
  [ "$status" -eq 0 ]
  [ "$output" == 'false' ]
}

@test 'word with duplicated character in mixed case, lowercase first' {
  run bash isogram.sh 'alphAbet'
  [ "$status" -eq 0 ]
  [ "$output" == 'false' ]
}

@test 'same first and last characters' {
  run bash isogram.sh 'angola'
  [ "$status" -eq 0 ]
  [ "$output" == 'false' ]
}
