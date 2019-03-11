local DNA = {}

function DNA:new(strand)
    local dna = {}
    setmetatable(dna, self)
    self.__index = self

    dna.nucleotideCounts = { A=0, T=0, C=0, G=0 }
    for char in (strand or ""):gmatch(".") do
        if dna.nucleotideCounts[char] then
            dna.nucleotideCounts[char] = dna.nucleotideCounts[char] + 1
        else
            error("Invalid Nucleotide: " .. char)
        end
    end
    return dna
end

function DNA:count(nucleotide)
    if self.nucleotideCounts[nucleotide] then
        return self.nucleotideCounts[nucleotide]
    else
        error("Invalid Nucleotide")
    end
end

return DNA
