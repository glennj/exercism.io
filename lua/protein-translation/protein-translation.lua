local proteins = {
    AUG = "Methionine",
    UUU = "Phenylalanine", UUC = "Phenylalanine",
    UUA = "Leucine", UUG = "Leucine",
    UCU = "Serine", UCC = "Serine", UCA = "Serine", UCG = "Serine",
    UAU = "Tyrosine", UAC = "Tyrosine",
    UGU = "Cysteine", UGC = "Cysteine",
    UGG = "Tryptophan",
    UAA = "STOP", UAG = "STOP", UGA = "STOP",
}

local codon = function(codon)
    assert(proteins[codon], "Unknown codon: "..codon)
    return proteins[codon]
end

local rna_strand = function(rna)
    local proteins = {}
    for i = 1, #rna, 3 do
        local cod = rna:sub(i, i+2)
        local protein = codon(cod)    -- allow error to propagate
        if protein == "STOP" then break end
        table.insert(proteins, protein)
    end
    return proteins
end

return { codon = codon, rna_strand = rna_strand }
