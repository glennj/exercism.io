defmodule Bucket do
  defstruct [:name, :size, amount: 0]

  @spec full?(Bucket) :: boolean
  def full?(bucket), do: bucket.amount === bucket.size

  @spec empty?(Bucket) :: boolean
  def empty?(bucket), do: bucket.amount === 0

  @spec fill(Bucket) :: Bucket
  def fill(bucket), do: %{bucket | amount: bucket.size}

  @spec empty(Bucket) :: Bucket
  def empty(bucket), do: %{bucket | amount: 0}

  @spec available(Bucket) :: non_neg_integer
  def available(bucket), do: bucket.size - bucket.amount

  # this modifies 2 maps: have to return a tuple of buckets
  @spec pour_from(Bucket, Bucket) :: {Bucket, Bucket}
  def pour_from(bucket, other) do
    quantity = min(bucket.amount, Bucket.available(other))

    {
      %{bucket | amount: bucket.amount - quantity},
      %{other | amount: other.amount + quantity}
    }
  end
end
