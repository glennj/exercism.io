class Allergies
  private

  attr_reader :code

  Allergen = Struct.new(:value, :name)

  ALLERGENS = [
    Allergen.new(0b00000001, 'eggs'),
    Allergen.new(0b00000010, 'peanuts'),
    Allergen.new(0b00000100, 'shellfish'),
    Allergen.new(0b00001000, 'strawberries'),
    Allergen.new(0b00010000, 'tomatoes'),
    Allergen.new(0b00100000, 'chocolate'),
    Allergen.new(0b01000000, 'pollen'),
    Allergen.new(0b10000000, 'cats')
  ].freeze

  public

  def initialize(code = 0)
    @code = code
  end

  def allergic_to?(thing)
    allergen = ALLERGENS.find { |a| a.name == thing }
    raise ArgumentError, "Unknown allergen #{thing}" if allergen.nil?
    has? allergen
  end

  def list
    ALLERGENS.select { |a| has? a }.map(&:name)
  end

  private

  def has?(allergen)
    (code & allergen.value).nonzero?
  end
end
