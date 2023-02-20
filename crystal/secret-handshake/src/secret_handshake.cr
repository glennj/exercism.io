# monkeypatch Int
struct Int
  def bit_set?(index : Int) : Bool
    bits_set?(1 << index)
  end
end

module SecretHandshake
  ACTIONS = ["wink", "double blink", "close your eyes", "jump"]

  def self.commands(input : Number) : Array(String)
    handshake = ACTIONS.each_with_index
                       .select {|action, i| input.bit_set? i}
                       .map {|action, i| action}
                       .to_a
    handshake.reverse! if input.bit_set? ACTIONS.size
    handshake
  end
end
