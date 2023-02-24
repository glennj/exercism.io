module ProteinTranslation
  def self.proteins(strand : String) : Array(String)
    proteins = strand.scan(/.{1,3}/)
                     .map {|m| CODON_TO_PROTEIN[m[0]]?}
                     .take_while &.!=("STOP")

    raise ArgumentError.new if proteins.any? &.nil?
    proteins.map &.to_s
  end

  CODON_TO_PROTEIN = {
    "AUG" => "Methionine",
    "UUU" => "Phenylalanine",
    "UUC" => "Phenylalanine",
    "UUA" => "Leucine",
    "UUG" => "Leucine",
    "UCU" => "Serine",
    "UCC" => "Serine",
    "UCA" => "Serine",
    "UCG" => "Serine",
    "UAU" => "Tyrosine",
    "UAC" => "Tyrosine",
    "UGU" => "Cysteine",
    "UGC" => "Cysteine",
    "UGG" => "Tryptophan",
    "UAA" => "STOP",
    "UAG" => "STOP",
    "UGA" => "STOP"
  }
end
