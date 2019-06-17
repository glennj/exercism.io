function count_nucleotides(strand::AbstractString)
    count = Dict('A' => 0, 'C' => 0, 'G' => 0, 'T' => 0)

    function incr(c::Char)
        if c in keys(count) 
            count[c] += 1
        else
            throw(DomainError(c, "invalid nucleotide"))
        end
    end

    foreach(incr, strand)
    count
end
