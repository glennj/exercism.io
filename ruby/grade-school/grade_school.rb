class School
  private

  attr_reader :school

  public

  def initialize
    @school = Hash.new { |h, k| h[k] = [] }
  end

  def students(grade)
    school[grade]
  end

  def add(name, grade)
    school[grade] << name
    school[grade].sort!
  end

  def students_by_grade
    school.keys
          .sort
          .reduce([]) do |result, grade|
            result << { grade: grade, students: students(grade) }
          end
  end
end
