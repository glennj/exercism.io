class SaveTheCow
  new: (@word) =>
    @guessed = {}
    @state = {
      remainingFailures: 9,
      state: 'Ongoing',
      maskedWord: '_'\rep #word
    }

  guess: (letters) =>
    for letter in *letters
      switch @state.state
        when 'Win' then error 'cannot guess after the game is won'
        when 'Lose' then error 'cannot guess after the game is lost'
        else @process letter
    @state

  process: (letter) =>
    if @guessed[letter] or not @word\find letter
      @state.remainingFailures -= 1
      if @state.remainingFailures < 0
        @state.state = 'Lose'
        @state.remainingFailures = 0
      
    @guessed[letter] = letter
    @state.maskedWord = @word\gsub '.', (c) -> @guessed[c] or '_'

    if not @state.maskedWord\find '_'
      @state.state = 'Win'
