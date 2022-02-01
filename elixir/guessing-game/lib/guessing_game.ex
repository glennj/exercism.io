defmodule GuessingGameWithCond do
  def compare(secret_number, guess \\ :no_guess) do
    cond do
      not is_integer(guess) -> "Make a guess"
      guess == secret_number -> "Correct"
      abs(guess - secret_number) == 1 -> "So close"
      guess > secret_number -> "Too high"
      true -> "Too low"
    end
  end
end

defmodule GuessingGame do
  def compare(secret_number, guess \\ :no_guess)
  def compare(_ecret_number, guess) when not is_integer(guess),           do: "Make a guess"
  def compare(secret_number, guess) when secret_number == guess,          do: "Correct"
  def compare(secret_number, guess) when abs(secret_number - guess) == 1, do: "So close"
  def compare(secret_number, guess) when guess > secret_number,           do: "Too high"
  def compare(secret_number, guess) when guess < secret_number,           do: "Too low"
end
