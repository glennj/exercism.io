defmodule CircularBuffer do
  @moduledoc """
  An API to a stateful process that fills and empties a circular buffer
  """

  use Agent

  defstruct [:capacity, data: [], count: 0]

  @doc """
  Create a new buffer of a given capacity
  """
  @spec new(capacity :: integer) :: {:ok, pid}
  def new(cap) do
    Agent.start_link(fn -> %CircularBuffer{capacity: cap} end)
  end

  @doc """
  Read the oldest entry in the buffer, fail if it is empty
  """
  @spec read(buffer :: pid) :: {:ok, any} | {:error, atom}
  def read(buffer) do
    Agent.get_and_update(buffer, fn state ->
      if state.count == 0 do
        {{:error, :empty}, state}
      else
        {{:ok, hd(state.data)},
         state
         |> Map.update!(:data, &tl/1)
         |> Map.update!(:count, &(&1 - 1))}
      end
    end)
  end

  @doc """
  Write a new item in the buffer, fail if is full
  """
  @spec write(buffer :: pid, item :: any) :: :ok | {:error, atom}
  def write(buffer, item) do
    Agent.get_and_update(buffer, fn state ->
      if state.count == state.capacity do
        {{:error, :full}, state}
      else
        {:ok,
         state
         |> Map.update!(:data, &(&1 ++ [item]))
         |> Map.update!(:count, &(&1 + 1))}
      end
    end)
  end

  @doc """
  Write an item in the buffer, overwrite the oldest entry if it is full
  """
  @spec overwrite(buffer :: pid, item :: any) :: :ok
  def overwrite(buffer, item) do
    case write(buffer, item) do
      {:error, :full} ->
        read(buffer)
        write(buffer, item)

      result ->
        result
    end
  end

  @doc """
  Clear the buffer
  """
  @spec clear(buffer :: pid) :: :ok
  def clear(buffer) do
    Agent.update(buffer, fn state -> %CircularBuffer{capacity: state.capacity} end)
  end
end
