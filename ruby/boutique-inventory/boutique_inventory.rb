class BoutiqueInventory
  PRICE_CHEAP_UPPER_LIMIT = 30
  
  def initialize(items)
    @items = items
  end

  def item_names
    items.map {|item| item[:name]}.sort!
  end

  def cheap
    items.select {|item| item[:price] < PRICE_CHEAP_UPPER_LIMIT}
  end

  def out_of_stock
    items.select {|item| item[:quantity_by_size].empty?}
  end

  def stock_for_item(name)
    (items.select {|item| item[:name] == name}.first || {})[:quantity_by_size]
  end

  def total_stock
    items.reduce(0) {|total, item| total + item[:quantity_by_size].values.sum}
  end

  private
  attr_reader :items
 end
