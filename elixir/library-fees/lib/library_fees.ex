defmodule LibraryFees do
  def datetime_from_string(string) do
    NaiveDateTime.from_iso8601!(string)
  end

  def before_noon?(datetime) do
    datetime
    |> NaiveDateTime.to_time()
    |> Time.diff(~T[12:00:00])
    < 0
  end

  def return_date(checkout_datetime) do
    days = 28 + if before_noon?(checkout_datetime), do: 0, else: 1

    checkout_datetime
    |> NaiveDateTime.to_date()
    |> Date.add(days)
  end

  def days_late(planned_return_date, actual_return_datetime) do
    days_late(Date.diff(actual_return_datetime, planned_return_date))
  end

  defp days_late(d) when d < 0, do: 0
  defp days_late(d), do: d

  def monday?(datetime) do
    Date.day_of_week(datetime) == 1
  end

  def calculate_late_fee(checkout, return, rate) do
    [dt_checkout, dt_return] = Enum.map([checkout, return], &datetime_from_string/1)
    late_days = return_date(dt_checkout) |> days_late(dt_return)
    apply_discount(late_days * rate, monday?(dt_return))
  end

  defp apply_discount(fee, true), do: trunc(fee * 0.5)
  defp apply_discount(fee, false), do: fee
end
