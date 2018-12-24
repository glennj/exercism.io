class Nucleotide
  def initialize(dna)
    raise ArgumentError if dna =~ /[^ACGT]/
    @histogram = {}
    %w[A C G T].each do |nucleotide|
      @histogram[nucleotide] = dna.count(nucleotide)
    end
  end

  singleton_class.send(:alias_method, :from_dna, :new)
  attr_reader :histogram

  def count(nucleotide)
    raise ArgumentError unless @histogram.key? nucleotide
    @histogram[nucleotide]
  end
end
