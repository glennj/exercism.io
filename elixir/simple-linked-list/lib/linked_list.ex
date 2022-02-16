# This is not a tuple-based solution: it is quite elegant to
# use a tuple to represent the list, but I find this solution
# quite interesting.
#
# I'm particularly happy that I've avoided using the Enum
# module at all.

defmodule LinkedList do
  defstruct [:value, :next, count: 0]

  # A guard macro: determine if the list is empty.
  defmacrop is_empty(list) do
    quote do
      is_nil(unquote(list).next)
    end
  end

  @opaque t :: %{value: any(), next: t() | nil, count: non_neg_integer()}

  @doc """
  Construct a new LinkedList
  """
  @spec new() :: t
  def new() do
    %__MODULE__{}
  end

  @doc """
  Push an item onto a LinkedList
 
  The list's head holds the most recently added element
  """
  @spec push(t, any()) :: t
  def push(list, elem) do
    %__MODULE__{value: elem, next: list, count: list.count + 1}
  end

  @doc """
  Counts the number of elements in a LinkedList
 
  We _can_ do the count by walking the list, which would be O(N).
  But by keeping the :count in the struct, and incrementing
  it when we push(), we have an O(1) operation.
  """
  @spec count(t) :: non_neg_integer()
  def count(list), do: list.count

  ## recursively:
  # def count(list), do: do_count(list, 0)
  # defp do_count(list, c) when is_empty(list), do: c
  # defp do_count(list, c), do: do_count(list.next, c + 1)

  @doc """
  Determine if a LinkedList is empty
  """
  @spec empty?(t) :: boolean()
  def empty?(list), do: is_empty(list)

  @doc """
  Get the value of a head of the LinkedList
  """
  @spec peek(t) :: {:ok, any()} | {:error, :empty_list}
  def peek(list) when is_empty(list), do: {:error, :empty_list}
  def peek(list), do: {:ok, list.value}

  @doc """
  Get tail of a LinkedList
  """
  @spec tail(t) :: {:ok, t} | {:error, :empty_list}
  def tail(list) when is_empty(list), do: {:error, :empty_list}
  def tail(list), do: {:ok, list.next}

  @doc """
  Remove the head from a LinkedList
  """
  @spec pop(t) :: {:ok, any(), t} | {:error, :empty_list}
  def pop(list) when is_empty(list), do: {:error, :empty_list}
  def pop(list), do: {:ok, list.value, list.next}

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
  def to_list(list) when is_empty(list), do: []
  def to_list(list) do
    prev = to_list(list.next)
    [list.value | prev]
  end

  @doc """
  Reverse a LinkedList
  """
  @spec reverse(t) :: t
  def reverse(list), do: do_reverse(list, new())

  defp do_reverse(list, reversed) when is_empty(list), do: reversed

  defp do_reverse(list, reversed) do
    do_reverse(list.next, push(reversed, list.value))
  end
end

# vim: set ft=elixir:
