function wordcount(sentence::AbstractString)::Dict{AbstractString, Integer}
    re = r"\b\w(?:[[:alnum:]']*\w)?\b"

    ## first take
    ## # extract the words from the sentence
    ## words = [ m.match for m in eachmatch(re, lowercase(sentence)) ]
    ## # create the histogram: this is not very efficient
    ## Dict([ (w, count(ww -> w == ww, words)) for w in words ]) 

    histogram = Dict{AbstractString, Integer}()
    for m in eachmatch(re, lowercase(sentence))
        word = m.match
        histogram[word] = get(histogram, word, 0) + 1
    end
    histogram

end
