defmodule ComplexNumbers do
  @typedoc """
  In this module, complex numbers are represented as a tuple-pair containing the real and
  imaginary parts.
  For example, the real number `1` is `{1, 0}`, the imaginary number `i` is `{0, 1}` and
  the complex number `4+3i` is `{4, 3}'.
  """
  @type complex :: {float, float}

  defguardp is_complex(c) when
    is_tuple(c) and
    tuple_size(c) == 2 and
    is_number(elem(c, 0)) and
    is_number(elem(c, 1))

  @doc """
  Return the real part of a complex number
  """
  @spec real(a :: complex) :: float
  def real({a, _b}), do: a

  @doc """
  Return the imaginary part of a complex number
  """
  @spec imaginary(a :: complex) :: float
  def imaginary({_a, b}), do: b

  defp to_complex(n) when is_number(n), do: {n, 0}

  @doc """
  Multiply two complex numbers, or a real and a complex number
  """
  @spec mul(a :: complex | float, b :: complex | float) :: complex
  def mul(a, b) when not is_complex(a), do: mul(to_complex(a), b)
  def mul(a, b) when not is_complex(b), do: mul(a, to_complex(b))

  def mul({a, b}, {c, d}) do
    {a * c - b * d, b * c + a * d}
  end

  @doc """
  Add two complex numbers, or a real and a complex number
  """
  @spec add(a :: complex | float, b :: complex | float) :: complex
  def add(a, b) when not is_complex(a), do: add(to_complex(a), b)
  def add(a, b) when not is_complex(b), do: add(a, to_complex(b))

  def add({a, b}, {c, d}) do
    {a + c, b + d}
  end

  @doc """
  Subtract two complex numbers, or a real and a complex number
  """
  @spec sub(a :: complex | float, b :: complex | float) :: complex
  def sub(a, b) when not is_complex(a), do: sub(to_complex(a), b)
  def sub(a, b) when not is_complex(b), do: sub(a, to_complex(b))

  def sub({a, b}, {c, d}) do
    {a - c, b - d}
  end

  @doc """
  Divide two complex numbers, or a real and a complex number
  """
  @spec div(a :: complex | float, b :: complex | float) :: complex
  def div(a, b) when not is_complex(a), do: __MODULE__.div(to_complex(a), b)
  def div(a, b) when not is_complex(b), do: __MODULE__.div(a, to_complex(b))

  def div({a, b}, {c, d}) do
    {
      (a * c + b * d) / (c ** 2 + d ** 2),
      (b * c - a * d) / (c ** 2 + d ** 2)
    }
  end

  @doc """
  Absolute value of a complex number
  """
  @spec abs(a :: complex) :: float
  def abs({a, b}) do
    :math.sqrt(a ** 2 + b ** 2)
  end

  @doc """
  Conjugate of a complex number
  """
  @spec conjugate(a :: complex) :: complex
  def conjugate({a, b}) do
    {a, -b}
  end

  @doc """
  Exponential of a complex number
  """
  @spec exp(a :: complex) :: complex
  def exp({a, b}) do
    mul(
      to_complex(:math.exp(1) ** a),
      {:math.cos(b), :math.sin(b)}
    )
  end
end
