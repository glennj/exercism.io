defmodule FreelancerRates do
  @hours_per_day 8
  @days_per_month 22

  def daily_rate(hourly_rate) do
    1.0 * hourly_rate * @hours_per_day
  end

  def apply_discount(before_discount, discount) do
    before_discount * (100 - discount) / 100
  end

  def monthly_rate(hourly_rate, discount) do
    apply_discount(daily_rate(hourly_rate) * @days_per_month, discount)
    |> Float.ceil
    |> trunc
  end

  def days_in_budget(budget, hourly_rate, discount) do
    budget / monthly_rate(hourly_rate, discount) * @days_per_month
    |> Float.floor(1)
  end
end
