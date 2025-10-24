include std/regex.e as re
include std/sequence.e as seq

-- a list of regular expressions to use in order.
-- each regex has 2 sets of capturing parentheses
constant regexes = {
    re:new("^()((?:[aeiou]|xr|yt).*)"), -- apple, xray, ytria
    re:new("^([^aeiouy]+)(y.*)"),       -- my, rhythm
    re:new("^([^aeiou]*qu)(.*)"),       -- quit, square
    re:new("^([^aeiou]+)(.*)")          -- pig, strength
}

public function translate(sequence phrase)
    sequence result = {}
    sequence words = seq:split(phrase)
    for i = 1 to length(words) do
        for r = 1 to length(regexes) do
            object m = re:matches(regexes[r], words[i])
            if not (atom(m) and m = ERROR_NOMATCH) then
                result &= {m[3] & m[2] & "ay"}
                exit
            end if
        end for
    end for

    return join(result, ' ')
end function
