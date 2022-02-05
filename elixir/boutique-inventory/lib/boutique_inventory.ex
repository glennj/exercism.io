defmodule BoutiqueInventory do
  def sort_by_price(inventory) do
    Enum.sort_by(inventory, & &1.price, :asc)
  end

  def with_missing_price(inventory) do
    Enum.filter(inventory, fn item -> is_nil(item.price) end)
  end

  def increase_quantity(item, count) do
    update_in(
      item,
      [:quantity_by_size],
      fn qs ->
        Enum.map(qs, fn {size, num} -> {size, num + count} end)
        |> Enum.into(%{})
      end
    )
  end

  def total_quantity(item) do
    Enum.reduce(
      item.quantity_by_size,
      0,
      fn {_, num}, total -> total + num end
    )
  end
end
