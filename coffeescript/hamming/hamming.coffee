class Hamming
  @distance: (strand1, strand2) ->
    throw new Error 'strands must be of equal length' if strand1.length isnt strand2.length

    (true for i in [0 ... strand1.length] when strand1[i] isnt strand2[i]).length

module.exports = Hamming
