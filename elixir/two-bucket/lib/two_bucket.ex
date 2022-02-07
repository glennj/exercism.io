defmodule TwoBucket do
  defstruct [:bucket_one, :bucket_two, :moves]

  @type t :: %TwoBucket{
          bucket_one: pos_integer,
          bucket_two: pos_integer,
          moves: pos_integer
        }

  @type result :: {:ok, TwoBucket.t()} | {:error, :impossible}

  @doc """
  Find the quickest way to fill a bucket with some amount of water from two buckets of specific sizes.
  """
  @spec measure(
          size_one :: pos_integer,
          size_two :: pos_integer,
          goal :: pos_integer,
          start_bucket :: :one | :two
        ) :: result
  def measure(size_one, size_two, goal, start_bucket) do
    if not valid(size_one, size_two, goal) do
      {:error, :impossible}
    else
      {first, second} = initialize(size_one, size_two, start_bucket)
      solve(first, second, goal)
    end
  end

  @spec valid(pos_integer, pos_integer, pos_integer) :: boolean
  defp valid(size1, size2, goal) do
    gcd = gcd(size1, size2)

    cond do
      size1 <= 0 or size2 <= 0 or goal <= 0 -> false
      max(size1, size2) < goal -> false
      gcd == 1 -> true
      rem(goal, gcd) == 0 -> true
      true -> false
    end
  end

  defp gcd(a, b) when b == 0, do: a
  defp gcd(a, b), do: gcd(b, rem(a, b))

  @spec initialize(pos_integer, pos_integer, atom) :: {Bucket, Bucket}
  defp initialize(size1, size2, start) do
    bucket1 = %Bucket{name: :one, size: size1}
    bucket2 = %Bucket{name: :two, size: size2}

    case start do
      :one -> {bucket1, bucket2}
      :two -> {bucket2, bucket1}
    end
  end

  @spec solve(Bucket, Bucket, pos_integer) :: result
  defp solve(first, second, goal) do
    # first move, full the start bucket
    do_solve(Bucket.fill(first), second, goal, 1)
  end

  @spec do_solve(Bucket, Bucket, pos_integer, pos_integer) :: result
  # possible second move, fill the other bucket if it's the right size
  defp do_solve(first, second, goal, 1) when second.size == goal do
    do_solve(first, Bucket.fill(second), goal, 2)
  end

  # has the goal been reached?
  defp do_solve(first, second, goal, moves)
       when first.amount == goal or
              second.amount == goal do

    {bucket_one, bucket_two} =
      case first.name do
        :one -> {first, second}
        :two -> {second, first}
      end

    {:ok,
     %TwoBucket{
       bucket_one: bucket_one.amount,
       bucket_two: bucket_two.amount,
       moves: moves
     }}
  end

  # make a move
  defp do_solve(first, second, goal, moves) do
    cond do
      Bucket.empty?(first) ->
        do_solve(Bucket.fill(first), second, goal, moves + 1)

      Bucket.full?(second) ->
        do_solve(first, Bucket.empty(second), goal, moves + 1)

      true ->
        {first, second} = Bucket.pour_from(first, second)
        do_solve(first, second, goal, moves + 1)
    end
  end
end
