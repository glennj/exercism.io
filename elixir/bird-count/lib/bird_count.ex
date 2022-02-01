defmodule BirdCount do
  @busy_threshold 5

  @doc "How many birds visited today"
  def today([]), do: nil

  def today([todays_count | _]), do: todays_count

  @doc "Hey, there's a new one. Add it to today's count"
  def increment_day_count([]), do: [1]

  def increment_day_count([todays_count | history]) do
    [1 + todays_count | history]
  end

  @doc "Has there ever been a day without a bird sighting?"
  def has_day_without_birds?([]), do: false

  def has_day_without_birds?([0 | _]), do: true

  def has_day_without_birds?([_ | history]), do: has_day_without_birds?(history)

  @doc "In history, how many birds have I seen"
  def total([]), do: 0

  def total([todays_count | history]), do: todays_count + total(history)

  @doc "How many 'busy' days"
  def busy_days([]), do: 0

  def busy_days([todays_count | history]) when todays_count >= @busy_threshold do
    1 + busy_days(history)
  end

  def busy_days([_ | history]), do: busy_days(history)
end
