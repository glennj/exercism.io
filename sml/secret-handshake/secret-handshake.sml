local
  val actions = Array.fromList ["wink", "double blink", "close your eyes", "jump"]

  fun isBitSet (number: int, offset: int): bool = 
    let open Word 
        val & = andb
        infix >>
        infix &
    in
       ((fromInt number >> fromInt offset) & 0w1) = 0w1
    end
in
  fun commands (number: int): string list =
    let fun addIf (i, action, acc) = if isBitSet (number, i) then action :: acc else acc
        val handshake              = Array.foldli addIf [] actions
    in  if isBitSet (number, Array.length actions)
        then handshake
        else List.rev handshake
    end
end