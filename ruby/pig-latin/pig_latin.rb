module PigLatin
  module_function

  def translate(phrase)
    phrase
      .split(' ')
      .map { |word| translate_word(word.downcase) }
      .join(' ')
  end

  def translate_word(word)
    # "apple" => "appleay", "xray" => "xrayay"
    m = word.match(/^(?:[aeiou]|xr|yt)/)
    return "#{word}ay" if m

    # 1. "square" => "aresquay", "quip" => "ipquay"
    # 2. "rhythm" => "ythmrhay", "my" => "ymay"
    # 3. "strengths" => "engthsstray"
    m = (
      word.match(/^(.?qu)(.*)/) ||
      word.match(/^([^aeiou]+)(y.*)/) ||
      word.match(/^([^aeiou]+)(.*)/)
    )
    return "#{m[2]}#{m[1]}ay" if m

    # are there any other cases?
    "#{word}ay"
  end
end
