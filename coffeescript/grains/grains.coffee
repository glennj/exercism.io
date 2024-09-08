class Grains
  SIZE = 64
  
  @square: (n) -> 
    if n < 1 or n > SIZE
      throw Error("square must be between 1 and #{SIZE}")
    2 ** (n - 1)

  @total: () -> 2 ** SIZE - 1

module.exports = Grains
