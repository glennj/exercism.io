function to_rna(dna::AbstractString)
    dna2rna = Dict(
        'C' => 'G',
        'G' => 'C',
        'T' => 'A',
        'A' => 'U',
    )
    
    function transcribe(x)
        x in keys(dna2rna) || error("invalid nucleotide")
        dna2rna[x]
    end

    map(transcribe, dna) |> join
end
