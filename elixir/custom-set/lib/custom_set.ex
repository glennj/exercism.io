defmodule CustomSet do
  @opaque t :: %__MODULE__{map: map}

  defstruct [map: %{}]

  @spec new(Enum.t()) :: t
  def new(enumerable) do
    Enum.reduce(enumerable, %__MODULE__{}, &(add(&2, &1)))
  end

  @spec add(t, any) :: t
  def add(custom_set, element) do
    Map.update!(custom_set, :map, &Map.put_new(&1, element, :ok))
  end

  @spec empty?(t) :: boolean
  def empty?(custom_set) do
    map_size(custom_set.map) == 0
  end

  @spec contains?(t, any) :: boolean
  def contains?(custom_set, element) do
    not is_nil(Map.get(custom_set.map, element))
  end

  @spec subset?(t, t) :: boolean
  def subset?(custom_set_1, custom_set_2) do
    Enum.all?(
      Map.keys(custom_set_1.map),
      &contains?(custom_set_2, &1)
    )
  end

  @spec disjoint?(t, t) :: boolean
  def disjoint?(custom_set_1, custom_set_2) do
    not Enum.any?(
      Map.keys(custom_set_1.map),
      &contains?(custom_set_2, &1)
    )
  end

  @spec equal?(t, t) :: boolean
  def equal?(custom_set_1, custom_set_2) do
    map_size(custom_set_1.map) == map_size(custom_set_2.map) 
    and subset?(custom_set_1, custom_set_2)
  end

  @spec intersection(t, t) :: t
  def intersection(custom_set_1, custom_set_2) do
    Enum.filter(
      Map.keys(custom_set_1.map),
      &contains?(custom_set_2, &1)
    )
    |> new()
  end

  @spec difference(t, t) :: t
  def difference(custom_set_1, custom_set_2) do
    Enum.filter(
      Map.keys(custom_set_1.map),
      &(not contains?(custom_set_2, &1))
    )
    |> new()
  end

  @spec union(t, t) :: t
  def union(custom_set_1, custom_set_2) do
    Enum.reduce(Map.keys(custom_set_1.map), custom_set_2, &add(&2, &1))
  end
end