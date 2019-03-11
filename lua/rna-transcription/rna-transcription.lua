local dna2rna = { G='C', C='G', T='A', A='U' }

local to_rna = function (dna)
    local rna = ""
    for char in dna:gmatch(".") do
        rna = rna .. dna2rna[char]
    end
    return rna
end

return to_rna
