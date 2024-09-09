class PigLatin
  regexes = [
    /^([^aeiou]*qu)(.+)/i        # quay, square
    /^([^aeiouy]+)(y.*)/i        # my, rhythm
    /^()((?:xr|yt|[aeiou]).+)/i  # apple, special cases xray, yttria
    /^([^aeiou]+)(.+)/i          # strength, cow
  ]

  @translate: (phrase) -> phrase.split(' ').map(@translateWord).join(' ')

  @translateWord: (word) ->
    for re in regexes
      if m = re.exec(word)
        return "#{m[2]}#{m[1]}ay"

      
module.exports = PigLatin
