function transform(ch)
    ch == '-'                    && return "_"
    'α' ≤ ch ≤ 'ω'               && return "?"
    isuppercase(ch)              && return "-$(lowercase(ch))"
    (isspace(ch) || isdigit(ch)) && return ""

    string(ch)
end

clean(str) = [transform(ch) for ch in str] |> join
