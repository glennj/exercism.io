include std/math.e
include std/sequence.e
include std/utils.e

constant actions = {"wink", "double blink", "close your eyes", "jump"}

public function commands(integer number)
  sequence handshake = {}
  for i = 1 to length(actions) do
    if and_bits(shift_bits(number, i - 1), 1) then
      handshake = append(handshake, actions[i])
    end if
  end for

  return iif(and_bits(shift_bits(number, length(actions)), 1),
              reverse(handshake),
              handshake)
end function
