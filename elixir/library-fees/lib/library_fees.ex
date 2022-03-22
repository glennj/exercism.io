defmodule LibraryFees do
  def datetime_from_string(string) do
    NaiveDateTime.from_iso8601!(string)
  end

  def before_noon?(datetime) do
    diff =
      datetime
      |> NaiveDateTime.to_time()
      |> Time.diff(~T[12:00:00])

    diff < 0
  end

  def return_date(checkout_datetime) do
    days = 28 + if before_noon?(checkout_datetime), do: 0, else: 1

    checkout_datetime
    |> NaiveDateTime.to_date()
    |> Date.add(days)
  end

  def days_late(planned_return_date, actual_return_datetime) do
    case Date.diff(actual_return_datetime, planned_return_date) do
      d when d < 0 -> 0
      d -> d
    end
  end

  def monday?(datetime), do: Date.day_of_week(datetime) == 1

  def calculate_late_fee(checkout, return, rate) do
    [checkout_datetime, return_datetime] = Enum.map([checkout, return], &datetime_from_string/1)

    late_days =
      return_date(checkout_datetime)
      |> days_late(return_datetime)

    fee = late_days * rate

    if monday?(return_datetime),
      do: trunc(fee * 0.5),
      else: fee
  end
end
