require 'date'

class Meetup
  FIRST_DAY = { first: 1, second: 8, third: 15, fourth: 22, fifth: 29,
                teenth: 13, last: nil }.freeze

  WEEKDAYS = %i[ sunday monday tuesday wednesday
                 thursday friday saturday ].freeze

  def initialize(month, year)
    @month = month
    @year = year
  end

  def day(weekday, nth)
    wday = WEEKDAYS.index weekday
    raise ArgumentError if wday.nil?
    raise ArgumentError unless FIRST_DAY.key? nth

    # start with the given date,
    date = if nth == :last
           then Date.new(@year, @month, 1).next_month - 7
           else Date.new(@year, @month, FIRST_DAY[nth])
           end
    # then increment until the desired day is found.
    date += 1 until date.wday == wday
    date
  end
end
