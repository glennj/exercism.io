module Bob
  module_function

  def hey(input)
    silent   = input.strip.empty?
    question = input.rstrip.end_with? '?'
    # contains at least 1 letter, but no lower case letters
    shouting = input =~ /^(?=.*[[:alpha:]])(?!.*[[:lower:]])/

    if question && shouting
      "Calm down, I know what I'm doing!"
    elsif question
      'Sure.'
    elsif shouting
      'Whoa, chill out!'
    elsif silent
      'Fine. Be that way!'
    else
      'Whatever.'
    end
  end
end
