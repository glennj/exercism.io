class Raindrops
  @drops: {3: "Pling", 5: "Plang", 7: "Plong"}
  
  @convert: (number) ->
    sounds = (sound for n, sound of @drops when number % n is 0)
    sounds.join('') or number.toString()    # empty string is falsy

module.exports = Raindrops
