class School
  def initialize
    @school = Hash.new { |h, k| h[k] = [] }
  end

  def students(grade)
    @school[grade]
  end

  def add(name, grade)
    @school[grade] << name
    @school[grade].sort!
  end

  def students_by_grade
    @school.keys
           .sort
           .reduce([]) do |result, grade|
             result << { grade: grade, students: students(grade) }
           end
  end
end

=begin

nifty community solution: subclass Hash
https://exercism.io/tracks/ruby/exercises/grade-school/solutions/bd9657b45fe14804928ddacb19a41a64

class School < Hash
  def initialize
    super { |h, k| h[k] = [] }
  end

  alias :students :[]

  def add(name, grade)
    self[grade] << name
    self[grade].sort!
  end

  def students_by_grade
    sort.map { |grade, kids| { grade: grade, students: kids } }
  end
end

=end