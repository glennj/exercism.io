{
  to_roman: (number) ->

    convert = (dec, roman) ->

      add_digit = (base, digits) ->
        if     dec >= 10 * base then convert dec - 10 * base, roman .. digits.big
        elseif dec >=  9 * base then convert dec +  1 * base, roman .. digits.low
        elseif dec >=  5 * base then convert dec -  5 * base, roman .. digits.mid
        elseif dec >=  4 * base then convert dec +  1 * base, roman .. digits.low
        else                         convert dec -  1 * base, roman .. digits.low

      if     dec >= 400 then add_digit 100, {big: 'M', mid: 'D', low: 'C'}
      elseif dec >=  40 then add_digit  10, {big: 'C', mid: 'L', low: 'X'}
      elseif dec >=   1 then add_digit   1, {big: 'X', mid: 'V', low: 'I'}
      else                   roman

    convert number, ''
}
