#!/usr/bin/env bash

# encode

@test "encode yes" {
  run bash atbash_cipher.sh encode "yes"
  [ "$status" -eq 0 ]
  [ "$output" == "bvh" ]
}

@test "encode no" {
  run bash atbash_cipher.sh encode "no"
  [ "$status" -eq 0 ]
  [ "$output" == "ml" ]
}

@test "encode OMG" {
  run bash atbash_cipher.sh encode "OMG"
  [ "$status" -eq 0 ]
  [ "$output" == "lnt" ]
}

@test "encode spaces" {
  run bash atbash_cipher.sh encode "O M G"
  [ "$status" -eq 0 ]
  [ "$output" == "lnt" ]
}

@test "encode mindblowingly" {
  run bash atbash_cipher.sh encode "mindblowingly"
  [ "$status" -eq 0 ]
  [ "$output" == "nrmwy oldrm tob" ]
}

@test "encode numbers" {
  run bash atbash_cipher.sh encode "Testing,1 2 3, testing."
  [ "$status" -eq 0 ]
  [ "$output" == "gvhgr mt123 gvhgr mt" ]
}

@test "encode deep thought" {
  run bash atbash_cipher.sh encode "Truth is fiction."
  [ "$status" -eq 0 ]
  [ "$output" == "gifgs rhurx grlm" ]
}

@test "encode all the letters" {
  run bash atbash_cipher.sh encode "The quick brown fox jumps over the lazy dog."
  [ "$status" -eq 0 ]
  [ "$output" == "gsvjf rxpyi ldmul cqfnk hlevi gsvoz abwlt" ]
}

# decode

@test "decode exercism" {
  run bash atbash_cipher.sh decode "vcvix rhn"
  [ "$status" -eq 0 ]
  [ "$output" == "exercism" ]
}

@test "decode a sentence" {
  run bash atbash_cipher.sh decode "zmlyh gzxov rhlug vmzhg vkkrm thglm v"
  [ "$status" -eq 0 ]
  [ "$output" == "anobstacleisoftenasteppingstone" ]
}

@test "decode numbers" {
  run bash atbash_cipher.sh decode "gvhgr mt123 gvhgr mt"
  [ "$status" -eq 0 ]
  [ "$output" == "testing123testing" ]
}

@test "decode all the letters" {
  run bash atbash_cipher.sh decode "gsvjf rxpyi ldmul cqfnk hlevi gsvoz abwlt"
  [ "$status" -eq 0 ]
  [ "$output" == "thequickbrownfoxjumpsoverthelazydog" ]
}

@test "decode with too many spaces" {
  run bash atbash_cipher.sh decode "vc vix    r hn"
  [ "$status" -eq 0 ]
  [ "$output" == "exercism" ]
}

@test "decode with no spaces" {
  run bash atbash_cipher.sh decode "zmlyhgzxovrhlugvmzhgvkkrmthglmv"
  [ "$status" -eq 0 ]
  [ "$output" == "anobstacleisoftenasteppingstone" ]
}

