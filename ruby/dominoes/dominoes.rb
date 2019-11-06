class Dominoes
  def self.chain?(input)
    return true if input.empty?

    dominoes = input.map { |pair| Domino.new(*pair) }

    dominoes.each_with_index do |d, i|
      rest = remove_index(dominoes, i)
      return true if chain_from?([d], rest)
      return true if chain_from?([d.reverse], rest)
    end
    false
  end

  ##########################################################
  def self.chain_from?(chain, remaining)
    return chain.first.head == chain.last.tail if remaining.empty?

    tail = chain.last.tail

    remaining.each_with_index do |d, i|
      next unless d.has?(tail)

      d.reverse! unless d.head == tail
      rest = remove_index(remaining, i)
      return true if chain_from?([*chain, d], rest)
    end
    false
  end

  def self.remove_index(list, idx)
    copy = list.dup
    copy.delete_at(idx)
    copy
  end

  private_class_method :chain_from?, :remove_index

  ##########################################################
  class Domino
    attr_reader :head, :tail

    def initialize(num1, num2)
      @head = num1
      @tail = num2
    end

    def reverse
      self.class.new(tail, head)
    end

    def reverse!
      @head, @tail = tail, head # rubocop:disable Style/ParallelAssignment
    end

    def has?(number)
      [head, tail].include? number
    end

    def ==(other)
      other.class == self.class && other.state == state
    end

    protected

    def state
      [head, tail].sort
    end
  end
  ##########################################################
end
