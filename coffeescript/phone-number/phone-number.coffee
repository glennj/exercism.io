class PhoneNumber
  constructor: (number) ->
    # remove valid non-digits
    @number = number.replace(/[() .-]/g, '').replace(/^\+/, '')

  clean: ->
    switch
      when @number.length < 10 then throw new Error 'must not be fewer than 10 digits'
      when @number.length > 11 then throw new Error 'must not be more than 11 digits'
      when @number.length == 11
        throw new Error '11 digits must start with 1' unless @number.startsWith('1')
        @number = @number[1..]

    throw new Error 'letters not permitted'      if /\p{Letter}/u.test @number
    throw new Error 'punctuations not permitted' if /\D/.test @number

    throw new Error 'area code cannot start with zero' if @number.startsWith '0'
    throw new Error 'area code cannot start with one'  if @number.startsWith '1'

    throw new Error 'exchange code cannot start with zero' if @number[3] is '0'
    throw new Error 'exchange code cannot start with one'  if @number[3] is '1'

    @number

module.exports = PhoneNumber
