const s:PROTEINS = #{
                  \ AUG: 'Methionine',
                  \ UUU: 'Phenylalanine', UUC: 'Phenylalanine',
                  \ UUA: 'Leucine', UUG: 'Leucine',
                  \ UCU: 'Serine', UCC: 'Serine', UCA: 'Serine', UCG: 'Serine',
                  \ UAU: 'Tyrosine', UAC: 'Tyrosine',
                  \ UGU: 'Cysteine', UGC: 'Cysteine',
                  \ UGG: 'Tryptophan',
                  \ UAA: 'STOP', UAG: 'STOP', UGA: 'STOP',
                  \ }

function! Proteins(strand) abort
    let proteins = []
    let s = a:strand
    while len(s) > 0
        let codon = s[0:2]
        let s = s[3:]
        if !s:PROTEINS->has_key(codon) | throw 'Invalid codon' | endif
        let protein = s:PROTEINS[codon]
        if protein == 'STOP' | break | endif
        eval proteins->add(protein)
    endwhile
    return proteins
endfunction
