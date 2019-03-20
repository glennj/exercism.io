class Domino
  def initialize(n1, n2)
    @head, @tail = n1, n2
  end
  attr_reader :head, :tail

  def reverse
    Domino.new(@tail, @head)
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

############################################################
class Dominoes
  class << self
    def chain?(input)
      return true if input.empty?

      dominoes = input.map {|pair| Domino.new(*pair)}

      dominoes.each_with_index do |d, i|
        rest = dominoes.dup
        rest.delete_at(i)
        return true if chain_from?([d], rest)
        return true if chain_from?([d.reverse], rest)
      end

      return false
    end

    private

    def chain_from?(chain, remaining)
      if remaining.empty? 
        return chain.first.head == chain.last.tail
      end

      n = chain.last.tail
      candidates = remaining.select {|d| d.has?(n)}

      candidates.each do |d|
        i = remaining.find_index(d)
        rest = remaining.dup
        rest.delete_at(i)
        d.reverse! unless d.head == n
        return true if chain_from?([*chain, d], rest)
      end

      return false
    end
  end 
end 

 #result = Dominoes.chain?([[1,2], [2,3], [3,1]])
 #p result
