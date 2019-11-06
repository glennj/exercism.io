module Bob
  module_function

  def hey(input)
    silent   = input.strip.empty?
    question = input.rstrip.end_with? '?'
    # contains at least 1 letter, but no lower case letters
    shouting = input =~ /^(?=.*[[:alpha:]])(?!.*[[:lower:]])/

    if    question && shouting then "Calm down, I know what I'm doing!"
    elsif question             then 'Sure.'
    elsif shouting             then 'Whoa, chill out!'
    elsif silent               then 'Fine. Be that way!'
    else                            'Whatever.'
    end
  end
end
