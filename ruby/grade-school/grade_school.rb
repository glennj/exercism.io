class School
  def initialize
    @directory = Hash.new { |h, k| h[k] = [] }
  end

  def add(name, grade)
    return false if include?(name)

    @directory[grade].append(name).sort!
    true
  end

  def include?(name)
    @directory.values.any? { |students| students.include? name }
  end

  def grade(grade)
    @directory[grade]
  end

  def roster
    @directory.keys.sort.reduce([]) { |result, grade| result + grade(grade) }
  end
end
