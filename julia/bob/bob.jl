function bob(stimulus::AbstractString)
    input = strip(stimulus)

    #issilence  = length(input) == 0
    issilence  = isempty(input)

    isquestion = endswith(input, "?")

    # shouted input contains letters but no lower case letters
    #isshouting = occursin(r"[[:alpha:]]", input) && 
    #            !occursin(r"[[:lower:]]", input)
    isshouting = any(isletter, input) && !any(islowercase, input)

    if issilence
        "Fine. Be that way!"
    elseif isquestion && isshouting
        "Calm down, I know what I'm doing!"
    elseif isquestion
        "Sure."
    elseif isshouting
        "Whoa, chill out!"
    else
        "Whatever."
    end
    
end
