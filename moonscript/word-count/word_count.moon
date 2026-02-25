{
  count_words: (sentence) ->
    count = {}
    for word in sentence\lower!\gmatch "%f[%w][%w']+"
      trimmed = word\gsub "'+$", ''
      count[trimmed] = (count[trimmed] or 0) + 1
    count
}
