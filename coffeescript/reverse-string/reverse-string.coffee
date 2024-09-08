class ReverseString  
  @reverse: (str) -> (str[i] for i in [str.length - 1 .. 0] by -1).join('')

module.exports = ReverseString
