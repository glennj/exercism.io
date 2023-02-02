local DNA = {}
DNA.__index = DNA

function DNA:new(strand)
    local counts = { A=0, T=0, C=0, G=0 }
    for char in (strand or ""):gmatch(".") do
        assert(counts[char], "Invalid Sequence")
        counts[char] = counts[char] + 1
    end

    local dna = { nucleotideCounts = counts }
    setmetatable(dna, self)
    return dna
end

function DNA:count(nucleotide)
    local n = self.nucleotideCounts[nucleotide]
    assert(n, "Invalid Nucleotide")
    return n
end

return DNA
