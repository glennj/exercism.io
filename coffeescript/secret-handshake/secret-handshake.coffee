Number::isBitSet = (i) -> (this >> i) & 1 is 1

class SecretHandshake
  actions = ['wink', 'double blink', 'close your eyes', 'jump']
  
  @commands: (number) ->
    handshake = (action for action, i in actions when number.isBitSet i)
    handshake.reverse() if number.isBitSet actions.length
    handshake

module.exports = SecretHandshake
