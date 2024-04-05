include std/sequence.e
include std/text.e

public function is_pangram(sequence str)
    sequence letters = remove_dups(filter(lower(str), STDFLTR_ALPHA), RD_SORT)
    return equal(letters, "abcdefghijklmnopqrstuvwxyz")
end function
