function matching_brackets(input) 
    stack = []
    brackets = Dict(
        ']' => '[',
        '}' => '{',
        ')' => '('
    )

    for c ∈ input
        if c ∈ values(brackets)
            push!(stack, c)
        elseif c ∈ keys(brackets)
            if isempty(stack) || brackets[c] ≠ pop!(stack)
                return false
            end
        end
    end

    isempty(stack)
end
