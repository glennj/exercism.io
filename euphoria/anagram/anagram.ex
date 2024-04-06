include std/text.e
include std/sort.e
include std/sequence.e

enum WORD, KEY

type word(sequence w)
    return length(w) = 2 and sequence(w[WORD]) and sequence(w[KEY])
end type


public function findAnagrams(sequence subject, sequence candidates)
    word subj = {lower(subject), key(subject)}

    return sort(filter(candidates, routine_id("filt"), subj))
end function


function filt(sequence candidate, word subj)
    word cand = {lower(candidate), key(candidate)}

    return not equal(cand[WORD], subj[WORD]) and equal(cand[KEY], subj[KEY])
end function

function key(sequence str)
    return sort(lower(str))
end function
