function transform(input::AbstractDict)
    # take 1, with explicit loops
    # 
    # result = Dict{AbstractChar, Int}()
    # for (value, letters) in pairs(input)
    #     for letter in letters
    #         result[lowercase(letter)] = value
    #     end
    # end
    # return result

    # take 2, with comprehensions
    Dict(
        lowercase(letter) => value
        for (value, letters) in pairs(input) 
        for letter in letters
    )
end

