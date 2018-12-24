class TwelveDays
  ChristmasDay = Struct.new(:day, :gift)

  DAYS = [
    ChristmasDay.new('first',    'a Partridge in a Pear Tree'),
    ChristmasDay.new('second',   'two Turtle Doves'),
    ChristmasDay.new('third',    'three French Hens'),
    ChristmasDay.new('fourth',   'four Calling Birds'),
    ChristmasDay.new('fifth',    'five Gold Rings'),
    ChristmasDay.new('sixth',    'six Geese-a-Laying'),
    ChristmasDay.new('seventh',  'seven Swans-a-Swimming'),
    ChristmasDay.new('eighth',   'eight Maids-a-Milking'),
    ChristmasDay.new('ninth',    'nine Ladies Dancing'),
    ChristmasDay.new('tenth',    'ten Lords-a-Leaping'),
    ChristmasDay.new('eleventh', 'eleven Pipers Piping'),
    ChristmasDay.new('twelfth',  'twelve Drummers Drumming')
  ].freeze

  def self.song
    new.verses(1..DAYS.length)
  end

  def verses(range)
    range.each
         .map { |i| verse(i) }
         .join "\n"
  end

  def verse(n)
    gifts = (n - 1).downto(0).map { |i| DAYS[i].gift }
    gifts << "and #{gifts.pop}" if n > 1
    format "On the %<day>s day of Christmas my true love gave to me: %<gifts>s.\n",
           day: DAYS[n - 1].day,
           gifts: gifts.join(', ')
  end
end
