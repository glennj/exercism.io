class BookStore

  def self.calculate_price(basket)
    price, discount_set = new(basket).best_discount
    price
  end

  BOOK_PRICE = 8.00

  DISCOUNTS = {
    5 => 0.25,
    4 => 0.20,
    3 => 0.10,
    2 => 0.05,
    1 => 0
  }.freeze

  def initialize(basket)
    @basket = basket
    @discounts = [[1] * basket.length]
    @discounts += find_discounts(basket)

    # pad each group of discounts to account for all the books
    @discounts.map do |d|
      (basket.length - d.sum).times do
        d << 1
      end
    end
  end

  def best_discount
    @discounts.map do |set|
      price = set.reduce(0) do |sum, size|
        sum + size * BOOK_PRICE * (1 - DISCOUNTS[size])
      end
      [price, set]
    end
    .min_by(&:first)
  end

  private

  def find_discounts(basket)
    return [] if basket.length < 2

    discounts = []
    max_discount = [basket.length, DISCOUNTS.keys.max].min
    max_discount.downto(2) do |disc|
      next unless contains?(basket, disc)
      discounts << [disc]
      rest = remove_group(basket, disc)
      find_discounts(rest).each do |d|
        discounts << [disc].concat(d)
      end
    end
    discounts
  end

  # see if the basket contains a group of given size
  def contains?(basket, size)
    basket.uniq.length >= size
  end

  def remove_group(basket, size)
    histogram = Hash.new { 0 }
    basket.each { |b| histogram[b] += 1 }
    books = histogram.keys.sort_by { |b| -histogram[b] }
    books[0...size].each { |b| histogram[b] -= 1 }

    # construct the new basket
    histogram
      .each_pair
      .each_with_object([]) do |(book, count), new_basket|
        new_basket.concat([book] * count)
      end
  end
end
