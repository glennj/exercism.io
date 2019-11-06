require 'date'

class Meetup
  private

  attr_reader :month, :year

  FIRST_DAY = { first: 1, second: 8, third: 15, fourth: 22, fifth: 29,
                teenth: 13, last: nil }.freeze

  WEEKDAYS = %i[ sunday monday tuesday wednesday
                 thursday friday saturday ].freeze

  public

  def initialize(month, year)
    @month = month
    @year = year
  end

  def day(weekday, nth)
    wday = WEEKDAYS.index weekday
    raise ArgumentError if wday.nil?
    raise ArgumentError unless FIRST_DAY.key? nth

    # start with the first "nth" date
    date = first_nth_date(nth)
    # then increment until the desired weekday is found.
    date += 1 until date.wday == wday
    date
  end

  private

  def first_nth_date(nth)
    if nth == :last
    then Date.new(year, month, 1).next_month - 7
    else Date.new(year, month, FIRST_DAY[nth])
    end
  end
end
