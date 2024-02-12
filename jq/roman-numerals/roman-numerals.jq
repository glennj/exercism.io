# input: a decimal number
# output: a string of roman numerals
#
# example:
#       11 | to_roman
#       => "XI"
#
def to_roman:

  # input: state object {n: aNumber, r: romanString}
  # output: state object with .n and .r adjusted
  #
  # example:
  #     {n: 1454, r: "M"} | _digitizer(100; "M"; "D"; "C")
  #     => {n: 454, r: "MM"}
  #
  #     {n: 454, r: "MM"} | _digitizer(100; "M"; "D"; "C")
  #     => {n: 554, r: "MMC"}
  #
  def _digitizer(base; tenDigit; fiveDigit; oneDigit):
    if   .n >= 10 * base then (.n -= 10 * base | .r += tenDigit)
    elif .n >=  9 * base then (.n +=  1 * base | .r += oneDigit)
    elif .n >=  5 * base then (.n -=  5 * base | .r += fiveDigit)
    elif .n >=  4 * base then (.n +=  1 * base | .r += oneDigit)
    else                      (.n -=  1 * base | .r += oneDigit)
    end
  ;

  def _to_roman:
    if .n == 0 then .r
    else
      if   .n > 399 then _digitizer(100; "M"; "D"; "C")
      elif .n > 39  then _digitizer(10;  "C"; "L"; "X")
      else               _digitizer(1;   "X"; "V"; "I")
      end
      | _to_roman
    end
  ;

  {n: ., r: ""} | _to_roman
;

##################
.number | to_roman
