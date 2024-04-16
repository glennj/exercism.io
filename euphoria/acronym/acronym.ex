include std/text.e
include std/types.e

enum SEEKING_ALPHA, SEEKING_NON_ALPHA

public function acronym(sequence s)
    sequence acr = ""
    atom state = SEEKING_ALPHA

    for i = 1 to length(s) do
        atom char = s[i]
        switch state do
            case SEEKING_ALPHA then
                if t_alpha(char) then
                    acr &= char
                    state = SEEKING_NON_ALPHA
                end if

            case SEEKING_NON_ALPHA then
                if not (t_alpha(char) or char = '\'') then
                    state = SEEKING_ALPHA
                end if
        end switch
    end for

    return upper(acr)
end function

/* ***********************************************
 * or with regular expressions
 *
include std/regex.e as re
include std/sequence.e

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
 *
 */
