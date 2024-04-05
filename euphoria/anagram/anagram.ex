include std/text.e
include std/sort.e
include std/sequence.e


public function findAnagrams(sequence subject, sequence candidates)
    sequence lc_subj = lower(subject)
    return sort(filter(candidates, routine_id("filt"), {lc_subj, sort(lc_subj)}))
end function

function filt(sequence candidate, sequence subj_data)
    sequence lc = lower(candidate)
    return not equal(lc, subj_data[1]) and equal(sort(lc), subj_data[2])
end function
