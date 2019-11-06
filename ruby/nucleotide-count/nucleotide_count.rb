class Nucleotide
  attr_reader :histogram

  singleton_class.send(:alias_method, :from_dna, :new)

  def initialize(dna)
    raise ArgumentError if dna =~ /[^ACGT]/
    @histogram = {}
    %w[A C G T].each do |nucleotide|
      @histogram[nucleotide] = dna.count(nucleotide)
    end
  end

  def count(nucleotide)
    raise ArgumentError unless histogram.key? nucleotide
    histogram[nucleotide]
  end
end
