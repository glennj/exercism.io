defmodule RationalNumbers do
  @type rational :: {integer, integer}

  @doc """
  Add two rational numbers
  """
  @spec add(a :: rational, b :: rational) :: rational
  def add({a1, a2}, {b1, b2}) do
    {a1 * b2 + b1 * a2, a2 * b2} |> reduce()
  end

  @doc """
  Subtract two rational numbers
  """
  @spec subtract(a :: rational, b :: rational) :: rational
  def subtract({a1, a2}, {b1, b2}) do
    {a1 * b2 - b1 * a2, a2 * b2} |> reduce()
  end

  @doc """
  Multiply two rational numbers
  """
  @spec multiply(a :: rational, b :: rational) :: rational
  def multiply({a1, a2}, {b1, b2}) do
    {a1 * b1, a2 * b2} |> reduce()
  end

  @doc """
  Divide two rational numbers
  """
  @spec divide_by(num :: rational, den :: rational) :: rational
  def divide_by({a1, a2}, {b1, b2}) do
    {a1 * b2, a2 * b1} |> reduce()
  end

  @doc """
  Absolute value of a rational number
  """
  @spec abs(a :: rational) :: rational
  def abs({num, den}) do
    {Kernel.abs(num), Kernel.abs(den)}
  end

  @doc """
  Exponentiation of a rational number by an integer
  """
  @spec pow_rational(a :: rational, n :: integer) :: rational
  def pow_rational({num, den}, n) when n >= 0 do
    {num ** n, den ** n} |> reduce()
  end

  def pow_rational({num, den}, n) do
    m = Kernel.abs(n)
    {den ** m, num ** m} |> reduce()
  end

  @doc """
  Exponentiation of a real number by a rational number
  """
  @spec pow_real(x :: integer, n :: rational) :: float
  def pow_real(x, {num, den}), do: nth_root(x ** num, den)

  # Find the n'th root of a number
  @spec nth_root(num :: integer, n :: integer) :: float
  defp nth_root(num, n) do
    :math.exp(1) ** (:math.log(num) / n)
  end

  @doc """
  Reduce a rational number to its lowest terms
  """
  @spec reduce(a :: rational) :: rational
  def reduce({0, _}), do: {0, 1}

  def reduce({num, den}) when den < 0, do: reduce({-num, -den})

  def reduce({num, den}) do
    g = gcd(num, den) |> Kernel.abs()
    {div(num, g), div(den, g)}
  end

  # Determine the Greatest Common Divisor of two numbers
  @spec gcd(a :: integer, b :: integer) :: integer
  defp gcd(a, 0), do: a
  defp gcd(a, b), do: gcd(b, rem(a, b))
end
