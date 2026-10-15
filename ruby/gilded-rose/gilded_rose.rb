# rubocop:disable Layout/EmptyLinesAroundClassBody
# frozen_string_literal: true

# Iteration Notes:
# 1. initial solution
# 2. changing a variable name
# 3. -
# 4. using `attr_accessor` and public/private blocks
# 5. use a dispatch hash instead of a Factory class
#    - split the file so the updater class are known in advance
#    - split the BackstagePass#update! method into smaller pieces
# 6. - dispatcher class inst var only uses accessors
#    - some extra whitespace for readability

require_relative 'item_updaters'

Item = Struct.new(:name, :sell_in, :quality)

# ----------------------------------------
class GildedRose

  class << self

    private

    attr_writer :dispatcher

    public

    attr_reader :dispatcher

  end

  private

  # a "factory" dispatcher (Ruby hashes maintain insertion order)
  self.dispatcher = {
    /^Conjured Aged Brie/ => ConjuredAgedBrie,
    /^Conjured Sulfuras/ => ConjuredSulfuras,
    /^Conjured backstage passes/ => ConjuredBackstagePass,
    /^Conjured/ => ConjuredBasic,
    /^Aged Brie/ => AgedBrie,
    /^Sulfuras/ => Sulfuras,
    /^Backstage passes/ => BackstagePass,
    // => BasicUpdater
  }

  attr_accessor :items

  def initialize(items)
    self.items = items
  end

  public

  def update!
    # we pass the item object to the updater, and the updater mutates it.
    items.each do |item|
      klass = self.class.dispatcher.find { |regex, _| regex.match?(item.name) }.last
      klass.new(item).update!
    end
  end
end

# rubocop:enable Layout/EmptyLinesAroundClassBody
