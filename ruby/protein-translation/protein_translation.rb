class InvalidCodonError < StandardError; end

module Translation
  Codon2Protein = {
    'AUG' => 'Methionine',
    'UUU' => 'Phenylalanine',
    'UUC' => 'Phenylalanine',
    'UUA' => 'Leucine',
    'UUG' => 'Leucine',
    'UCU' => 'Serine',
    'UCC' => 'Serine',
    'UCA' => 'Serine',
    'UCG' => 'Serine',
    'UAU' => 'Tyrosine',
    'UAC' => 'Tyrosine',
    'UGU' => 'Cysteine',
    'UGC' => 'Cysteine',
    'UGG' => 'Tryptophan',
    'UAA' => 'STOP',
    'UAG' => 'STOP',
    'UGA' => 'STOP'
  }.freeze

  module_function

  def of_codon(codon)
    raise InvalidCodonError unless Codon2Protein.key? codon
    
    Codon2Protein[codon]
  end

  def of_rna(strand)
    proteins = []
    strand.scan(/.../).each do |codon|
      raise InvalidCodonError unless Codon2Protein.key? codon
      break if Codon2Protein[codon] == 'STOP'

      proteins << Codon2Protein[codon]
    end
    proteins
  end
end
