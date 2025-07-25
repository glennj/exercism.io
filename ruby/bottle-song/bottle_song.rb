module BottleSong
  private

  def self.verse(n)
    <<~VERSE
      #{bottle n},
      #{bottle n},
      And if one green bottle should accidentally fall,
      There'll be #{bottle(n-1).downcase}.
    VERSE
  end

  NUMS = %w[No One Two Three Four Five Six Seven Eight Nine Ten]

  def self.bottle(n)
    "#{NUMS[n]} green bottle#{n == 1 ? "" : "s"} hanging on the wall"
  end

  public

  def self.recite(start, count)
    start.downto(start - count + 1)
      .to_a
      .inject([]) {|verses, n| verses << verse(n)}
      .join "\n"
  end
end
