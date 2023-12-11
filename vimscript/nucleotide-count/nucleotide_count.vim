function! NucleotideCounts(strand) abort
  let result = #{A: 0, C: 0, G: 0, T: 0}
  for char in a:strand->split('\zs')
    if !result->has_key(char)
      throw 'Invalid nucleotide in strand'
    endif
    let result[char] += 1
  endfor
  return result
endfunction
