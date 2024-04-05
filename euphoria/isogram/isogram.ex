include std/sequence.e
include std/text.e

public function isogram( sequence str )
  sequence cleaned = filter(str, STDFLTR_ALPHA) -- keep only alphas
  atom uniqLen = length(remove_dups(lower(cleaned), RD_INPLACE))
  return uniqLen = length(cleaned)
end function
