String::reversed = () -> this.split('').reverse().join('')

A = "A".charCodeAt(0)


class Diamond
  @rows: (letter) ->
    n = letter.charCodeAt(0) - A + 1
    diamond = []

    for i from [0...n]
      chars = new Array(n).fill(' ')
      chars[i] = String.fromCharCode(A + i)
      rightHalf = chars.join('')
      diamond.push rightHalf.reversed() + rightHalf[1..]

    [diamond..., diamond.reverse()[1..]...].join("\n")


module.exports = Diamond
