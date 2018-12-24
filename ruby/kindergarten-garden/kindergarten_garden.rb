class Garden
  PLANTS = {
    'C' => :clover,   'G' => :grass,
    'R' => :radishes, 'V' => :violets
  }.freeze

  DEF_STUDENTS = %w[
    Alice Bob Charlie David Eve Fred
    Ginny Harriet Ileana Joseph Kincaid Larry
  ].freeze

  def initialize(diagram, students = DEF_STUDENTS)
    garden = parse diagram
    students = students.map(&:downcase).sort

    @holdings = students.slice(0, garden.length)
                        .zip(garden)
                        .to_h

    students.each do |student|
      define_singleton_method(:"#{student}") { @holdings[student] }
    end
  end

  private

  def parse(garden)
    rows = garden.lines.map { |line| line.chomp.scan(/../) }
    patches = Array.new(rows.first.length) { '' }
    rows.each do |row|
      row.each_with_index do |bit, i|
        patches[i] << bit
      end
    end
    patches.map { |patch| patch.chars.map { |c| PLANTS[c] } }
  end
end
