ordinal = (n) ->
  suffix = switch n % 100
    when 11, 12, 13 then 'th'
    else
      switch n % 10
        when 1 then 'st'
        when 2 then 'nd'
        when 3 then 'rd'
        else 'th'
  "#{n}#{suffix}"


class Lineup
  @format: (name, number) ->
    "#{name}, you are the #{ordinal number} customer we serve today. Thank you!"


module.exports = Lineup
