class InvalidCodonError < StandardError; end

module Translation
  CODON_TO_PROTEIN = {
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
    raise InvalidCodonError unless CODON_TO_PROTEIN.key? codon

    CODON_TO_PROTEIN[codon]
  end

  def of_rna(strand)
    proteins = []
    strand.scan(/.{1,3}/).each do |codon|
      raise InvalidCodonError unless CODON_TO_PROTEIN.key? codon
      break if CODON_TO_PROTEIN[codon] == 'STOP'

      proteins << CODON_TO_PROTEIN[codon]
    end
    proteins
  end
end
