# comment
module Complement
  module_function

  def of_dna(rna)
    rna.tr('CGTA', 'GCAU')
  end
end
