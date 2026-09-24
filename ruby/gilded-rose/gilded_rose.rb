# rubocop:disable Style/OneClassPerFile, Style/Documentation

# frozen_string_literal: true

# ----------------------------------------
Item = Struct.new(:name, :sell_in, :quality)

class GildedRose
  def initialize(items)
    @items = items
    @updater = ItemUpdaterFactory.new
  end

  def update!
    # we pass the item object to the updater, and the updater mutates it.
    @items.each { |item| @updater.item_updater(item).update! }
  end
end

# ----------------------------------------

# rubocop:disable-next Metrics/CyclomaticComplexity
class ItemUpdaterFactory
  def item_updater(item)
    case item.name
    when /^Conjured Aged Brie/        then ConjuredAgedBrie.new(item)
    when /^Conjured Sulfuras/         then ConjuredSulfuras.new(item)
    when /^Conjured backstage passes/ then ConjuredBackstagePass.new(item)
    when /^Conjured/                  then ConjuredBasic.new(item)
    when /^Aged Brie/                 then AgedBrie.new(item)
    when /^Sulfuras/                  then Sulfuras.new(item)
    when /^Backstage passes/          then BackstagePass.new(item)
    else                                   BasicUpdater.new(item)
    end
  end
end

class BasicUpdater
  def initialize(item)
    @item = item
    @delta = 1
  end

  def update!
    delta = @delta
    delta *= 2 if @item.sell_in <= 0
    @item.sell_in -= 1
    @item.quality = [0, @item.quality - delta].max
  end
end

class ConjuredBasic < BasicUpdater
  def initialize(item)
    super
    @delta = 2
  end

  def update!
    @item.sell_in -= 1
    @item.quality =
      if @item.sell_in.negative? then 0
      else [0, @item.quality - @delta].max
      end
  end
end

class AgedBrie < BasicUpdater
  def update!
    delta = @delta
    delta *= 2 if @item.sell_in <= 0
    @item.sell_in -= 1
    @item.quality = [50, @item.quality + delta].min
  end
end

class ConjuredAgedBrie < AgedBrie
  def update!
    super
    @item.quality = 0 if @item.sell_in.negative?
  end
end

class Sulfuras < BasicUpdater
  def update!; end
end

class ConjuredSulfuras < ConjuredBasic
  def initialize(item)
    super
    @delta = 0
  end
end

class BackstagePass < BasicUpdater
  def initialize(item)
    super
    @delta = { 5 => 3, 10 => 2, :default => 1 }
  end

  def update!
    quality =
      if    @item.sell_in <= 0  then 0
      elsif @item.sell_in <= 5  then @item.quality + @delta[5]
      elsif @item.sell_in <= 10 then @item.quality + @delta[10]
      else                           @item.quality + @delta[:default]
      end
    @item.quality = [50, quality].min
    @item.sell_in -= 1
  end
end

class ConjuredBackstagePass < BackstagePass
  def initialize(item)
    super
    @delta = { 5 => 2, 10 => 1, :default => 0 }
  end
end

# rubocop:enable Style/OneClassPerFile, Style/Documentation
