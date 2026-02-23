return (num) ->
  drops = ''
  drops ..= 'Pling' if num % 3 == 0
  drops ..= 'Plang' if num % 5 == 0
  drops ..= 'Plong' if num % 7 == 0
  #drops > 0 and drops or tostring num
