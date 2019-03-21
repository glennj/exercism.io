class Dominoes
  def self.chain?(input)
    return true if input.empty?

    dominoes = input.map {|pair| Domino.new(*pair)}

    dominoes.each_with_index do |d, i|
      rest = remove_index(dominoes, i)
      return true if chain_from?([d], rest)
      return true if chain_from?([d.reverse], rest)
    end
    false
  end

  ##########################################################
  private

  def self.chain_from?(chain, remaining)
    if remaining.empty? 
      return chain.first.head == chain.last.tail
    end
    tail = chain.last.tail

    remaining.each_with_index do |d, i|
      next unless d.has?(tail)
      d.reverse! unless d.head == tail
      rest = remove_index(remaining, i)
      return true if chain_from?([*chain, d], rest)
    end
    false
  end

  def self.remove_index(list, i)
    copy = list.dup
    copy.delete_at(i)
    copy
  end

  ##########################################################
  class Domino
    def initialize(n1, n2)
      @head, @tail = n1, n2
    end
    attr_reader :head, :tail

    def reverse
      self.class.new(@tail, @head)
    end

    def reverse!
      @head, @tail = @tail, @head
    end

    def has?(n)
      @head == n || @tail == n
    end

    def ==(obj)
      obj.class == self.class && obj.state == self.state
    end

    protected

    def state
      [@head, @tail].sort
    end
  end
  ##########################################################
end 
