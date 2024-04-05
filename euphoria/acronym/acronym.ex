include std/regex.e as re
include std/sequence.e
include std/text.e

-- match a "word" and capture the first letter
constant matcher = re:new(`([[:alpha:]])[[:alpha:]']*`)

public function acronym(sequence s)
    sequence matches = re:all_matches(matcher, s)
    sequence letters = apply(matches, routine_id("get_letter"), {}) 
    return upper(join(letters, ""))
end function

function get_letter(sequence match, object _)
    -- the captured letter is the 2nd element of the match
    return match[2]
end function
