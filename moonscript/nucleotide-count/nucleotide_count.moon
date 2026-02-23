counter = (strand) ->
  counts = A: 0, C: 0, G: 0, T: 0
  
  for nucleotide in strand\gmatch(".")
    assert counts[nucleotide], 'Invalid nucleotide in strand'
    counts[nucleotide] += 1

  counts

return counter
