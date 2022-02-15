defmodule LinkedList do
  @opaque t :: tuple()

  @doc """
  Construct a new LinkedList
  """
  @spec new() :: t
  def new() do
    {}
  end

  @doc """
  Push an item onto a LinkedList

  The list's head holds the most recently added element
  """
  @spec push(t, any()) :: t
  def push(list, elem) do
    {elem, list}
  end

  @doc """
  Counts the number of elements in a LinkedList
  """
  @spec count(t) :: non_neg_integer()
  def count(list), do: do_count(list, 0)

  defp do_count({}, c), do: c
  defp do_count({_, tail}, c), do: do_count(tail, c + 1)

  @doc """
  Determine if a LinkedList is empty
  """
  @spec empty?(t) :: boolean()
  def empty?({}), do: true
  def empty?(_), do: false

  @doc """
  Get the value of a head of the LinkedList
  """
  @spec peek(t) :: {:ok, any()} | {:error, :empty_list}
  def peek({}), do: {:error, :empty_list}
  def peek({value, _}), do: {:ok, value}

  @doc """
  Get tail of a LinkedList
  """
  @spec tail(t) :: {:ok, t} | {:error, :empty_list}
  def tail({}), do: {:error, :empty_list}
  def tail({_, tail}), do: {:ok, tail}

  @doc """
  Remove the head from a LinkedList
  """
  @spec pop(t) :: {:ok, any(), t} | {:error, :empty_list}
  def pop({}), do: {:error, :empty_list}
  def pop({value, tail}), do: {:ok, value, tail}

  @doc """
  Construct a LinkedList from a stdlib List

  Recursive, but not tail recursive.
  """
  @spec from_list(list()) :: t
  def from_list([]), do: new()

  def from_list([elem | elems]) do
    list = from_list(elems)
    push(list, elem)
  end

  @doc """
  Construct a stdlib List LinkedList from a LinkedList

  Recursive, but not tail recursive.
  """
  @spec to_list(t) :: list()
  def to_list({}), do: []

  def to_list({value, tail}) do
    prev = to_list(tail)
    [value | prev]
  end

  @doc """
  Reverse a LinkedList
  """
  @spec reverse(t) :: t
  def reverse(list), do: do_reverse(list, new())

  defp do_reverse({}, reversed), do: reversed

  defp do_reverse({value, tail}, reversed) do
    do_reverse(tail, push(reversed, value))
  end
end
