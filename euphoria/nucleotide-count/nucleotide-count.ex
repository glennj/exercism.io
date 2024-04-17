include std/map.e

enum FALSE=0, TRUE

constant empty_count = map:new_from_kvpairs({{'A', 0}, {'C', 0}, {'G', 0}, {'T', 0}})

public function counts(sequence nucleotides)
  map count = map:copy(empty_count)
  for i = 1 to length(nucleotides) do
    atom char = nucleotides[i]
    if not has(count, char) then return FALSE end if
    put(count, char, 1, map:ADD)
 end for
  return count
end function
