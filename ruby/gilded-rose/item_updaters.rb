# rubocop:disable Style/OneClassPerFile, Style/Documentation, Layout/EmptyLinesAroundClassBody
# frozen_string_literal: true

class BasicUpdater

  private

  attr_accessor :item, :delta

  def initialize(item)
    self.item = item
    self.delta = 1
  end

  public

  def update!
    self.delta *= 2 if item.sell_in <= 0
    item.sell_in -= 1
    item.quality = [0, item.quality - delta].max
  end
end

# ------------------------------------------------------------
class ConjuredBasic < BasicUpdater

  private

  def initialize(item)
    super
    self.delta = 2
  end

  public

  def update!
    item.sell_in -= 1
    item.quality =
      if item.sell_in.negative? then 0
      else [0, item.quality - delta].max
      end
  end
end

# ------------------------------------------------------------
class AgedBrie < BasicUpdater
  def update!
    self.delta *= 2 if item.sell_in <= 0
    item.sell_in -= 1
    item.quality = [50, item.quality + delta].min
  end
end

# ------------------------------------------------------------
class ConjuredAgedBrie < AgedBrie
  def update!
    super
    item.quality = 0 if item.sell_in.negative?
  end
end

# ------------------------------------------------------------
class Sulfuras < BasicUpdater
  def update!; end
end

# ------------------------------------------------------------
class ConjuredSulfuras < ConjuredBasic

  private

  def initialize(item)
    super
    self.delta = 0
  end
end

# ------------------------------------------------------------
class BackstagePass < BasicUpdater

  private

  def initialize(item)
    super
    self.delta = { 5 => 3, 10 => 2, :default => 1 }
  end

  public

  def update!
    item.quality = [50, next_quality].min
    item.sell_in -= 1
  end

  private

  def next_quality
    return 0 if item.sell_in <= 0

    item.quality + quality_delta
  end

  def quality_delta
    case item.sell_in
    when 1..5  then delta[5]
    when 6..10 then delta[10]
    else            delta[:default]
    end
  end
end

# ------------------------------------------------------------
class ConjuredBackstagePass < BackstagePass
  def initialize(item)
    super
    self.delta = { 5 => 2, 10 => 1, :default => 0 }
  end
end

# rubocop:enable Style/OneClassPerFile, Style/Documentation, Layout/EmptyLinesAroundClassBody
