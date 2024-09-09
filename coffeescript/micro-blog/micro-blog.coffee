class Microblog
  @truncate: (phrase) -> phrase.match(/^(.{1,5})/u)[1]
  # .............................................^

module.exports = Microblog
