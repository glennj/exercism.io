class Bob
  hey: (message) ->
    isEmpty = message.trim().length is 0
    isYelling = /\p{Upper}/u.test(message) and not /\p{Lower}/u.test(message)
    isQuestion = message.trimEnd().endsWith('?')

    switch
      when isYelling and isQuestion then "Calm down, I know what I'm doing!"
      when isYelling then "Whoa, chill out!"
      when isQuestion then "Sure."
      when isEmpty then "Fine. Be that way!"
      else "Whatever."

module.exports = Bob
