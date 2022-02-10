defmodule Meetup do
  @type weekday :: :monday | :tuesday | :wednesday | :thursday | :friday | :saturday | :sunday
  @type schedule :: :first | :second | :third | :fourth | :last | :teenth

  @weekdays %{monday: 1, tuesday: 2, wednesday: 3, thursday: 4, friday: 5, saturday: 6, sunday: 7}
  @start_day %{first: 1, second: 8, third: 15, fourth: 22, teenth: 13}

  @doc """
  Calculate a meetup date.
  """
  @spec meetup(pos_integer, pos_integer, weekday, schedule) :: Date.t()
  def meetup(year, month, weekday, schedule) do
    # if schedule is not in the @start_day map, then it is :last
    start_day = Map.get(@start_day, schedule, last_day_of_month(year, month) - 6)

    do_meetup(Date.new!(year, month, start_day), @weekdays[weekday])
  end

  defp do_meetup(date, dow) do
    if Date.day_of_week(date) == dow do
      date
    else
      do_meetup(Date.add(date, 1), dow)
    end
  end

  defp last_day_of_month(y, m) do
    date = first_day_of_next_month(y, m) |> Date.add(-1)
    date.day
  end

  defp first_day_of_next_month(y, 12), do: Date.new!(y + 1, 1, 1)
  defp first_day_of_next_month(y, m), do: Date.new!(y, m + 1, 1)
end
