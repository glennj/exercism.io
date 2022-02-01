defmodule Secrets do
  def secret_add(secret) do
    #fn x ->
    #  x + secret
    #end
    &(&1 + secret)
  end

  def secret_subtract(secret) do
    &(&1 - secret)
  end

  def secret_multiply(secret) do
    &(&1 * secret)
  end

  def secret_divide(secret) do
    fn x ->
      x / secret |> trunc
    end
  end

  def secret_and(secret) do
    fn x ->
      Bitwise.&&&(x, secret)
    end
  end

  def secret_xor(secret) do
    &( Bitwise.^^^(&1, secret) )
  end

  def secret_combine(secret_function1, secret_function2) do
    fn param ->
      #secret_function2.(secret_function1.(param))

      #secret_function1.(param) |> secret_function2.()

      param
      |> secret_function1.()
      |> secret_function2.()
    end
  end
end
