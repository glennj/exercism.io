require 'ostruct'

class BoutiqueInventory
  attr_reader :items

  def initialize(items)
    @items = items.map {|item| OpenStruct.new(item)}
  end

  def item_names
    items.map(&:name).sort
  end

  def total_stock
    items.reduce(0) {|total, item| total + item.quantity_by_size.values.sum}
    # or:
    # items.map(&:quantity_by_size)
    #      .map(&:values)
    #      .map(&:sum)
    #      .sum
  end
end
