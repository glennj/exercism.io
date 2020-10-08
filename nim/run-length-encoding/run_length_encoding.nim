# https://github.com/flaviut/nre

import nre, strutils

proc encode*(input: string): string =
  input.replace(
    re"(.)(\1+)",
    proc (m: RegexMatch): string =
      $(1 + m.captures[1].len) & m.captures[0]
  )


proc decode*(input: string): string =
  input.replace(
    re"(\d+)(\D)",
    proc (m: RegexMatch): string =
      m.captures[1].repeat(m.captures[0].parseInt)
  )


############################################################
# Alternative implementations:
############################################################
# Using `skipWhile`
#[
import parseutils, strutils, strformat

proc encode*(input: string): string =
  result = ""
  var i = 0
  var count: int
  while i < input.len:
    count = input.skipWhile({input[i]}, i)
    if count > 1:
      result.add(&"{$count}{input[i]}")
    else:
      result.add(input[i])
    i.inc(count)

proc decode*(input: string): string =
  result = ""
  var i = 0
  var count: int
  while i < input.len:
    count = input.skipWhile({'0'..'9'}, i)
    if count == 0:
      result.add(input[i])
    else:
      result.add(input[i+count].repeat(input[i ..< i+count].parseInt))
      i.inc(count)
    i.inc
]#

############################################################
# Using builtin `re` package
#[
import re, strutils

proc encode*(input:string): string =
  result = input
  var r = re"^(.*?)(.)(\2+)(.*)"
  var m: array[4, string]
  while result.match(r, m):
    result = m[0] & $(1 + m[2].len) & m[1] & m[3]

proc decode*(input: string): string =
  result = input
  var r = re"^(.*?)(\d+)(.)(.*)"
  var m: array[4, string]
  while result.match(r, m):
    result = m[0] & m[2].repeat(m[1].parseInt) & m[3]
]#
