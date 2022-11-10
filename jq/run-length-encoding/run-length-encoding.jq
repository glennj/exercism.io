def encode:
  reduce match("(.)\\1*"; "g") as $m (""; 
    . + if $m.length == 1 then "" else $m.length | tostring end
      + $m.captures[0].string
  )
;

def decode:
  reduce match("(\\d*)(\\D)"; "g") as $m (""; 
    (if $m.captures[0].length == 0
      then 1
      else $m.captures[0].string | tonumber
    end) as $len
    | . + ($m.captures[1].string * $len)
  )
;

################################################################
# `match` with the "g" flag outputs a stream of "match objects"
#
#    $ jq -n '"ABBCCC" | match("(.)\\1*"; "g")'
#    {
#      "offset": 0,
#      "length": 1,
#      "string": "A",
#      "captures": [
#        {
#          "offset": 0,
#          "length": 1,
#          "string": "A",
#          "name": null
#        }
#      ]
#    }
#    {
#      "offset": 1,
#      "length": 2,
#      "string": "BB",
#      "captures": [
#        {
#          "offset": 1,
#          "length": 1,
#          "string": "B",
#          "name": null
#        }
#      ]
#    }
#    {
#      "offset": 3,
#      "length": 3,
#      "string": "CCC",
#      "captures": [
#        {
#          "offset": 3,
#          "length": 1,
#          "string": "C",
#          "name": null
#        }
#      ]
#    }
