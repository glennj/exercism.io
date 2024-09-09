class RnaTranscription
  constructor: (@dna) ->

  toRna: -> 
    unless @rna?
      @rna = [@dna...].map(@complement).join('')
    @rna

  complement: (nucleotide) ->
    switch nucleotide
      when 'G' then 'C'
      when 'C' then 'G'
      when 'T' then 'A'
      when 'A' then 'U'
      
module.exports = RnaTranscription
