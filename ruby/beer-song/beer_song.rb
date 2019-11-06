class BeerSong
  private

  attr_reader :max
  attr_reader :liquid
  attr_reader :where

  public

  def self.recite(start, num)
    new.verses(start, num)
  end

  def initialize(max: 99, liquid: 'beer', where: 'on the wall')
    @max = max
    @liquid = liquid
    @where = where
  end

  def verses(start, num)
    stop = start - num + 1
    start.downto(stop).map { |n| verse(n) }.join("\n")
  end

  private

  def verse(n)
    b = bottle(n)
    first = "#{b.capitalize} #{where}, #{b}.\n"

    b = bottle(n.positive? ? n - 1 : max)
    second = "#{task(n)}, #{b} #{where}.\n"

    first + second
  end

  def bottle(n)
    "#{n.zero? ? 'no more' : n} bottle#{n == 1 ? '' : 's'} of #{liquid}"
  end

  def task(n)
    if n.zero?
      'Go to the store and buy some more'
    else
      "Take #{n == 1 ? 'it' : 'one'} down and pass it around"
    end
  end
end
