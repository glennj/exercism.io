defmodule BoutiqueInventory do
  def sort_by_price(inventory) do
    Enum.sort_by(inventory, & &1.price, :asc)
  end

  def with_missing_price(inventory) do
    Enum.filter(inventory, &is_nil(&1.price))
  end

  def update_names(inventory, search, replace) do
    Enum.map(
      inventory,
      fn item ->
        %{item | name: String.replace(item.name, search, replace)}
      end
    )
  end

  def increase_quantity(item, count) do
    update_in(
      item,
      [:quantity_by_size],
      fn qs ->
        Map.new(
          qs,
          fn {size, num} -> {size, num + count} end
        )
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
