structure CircularBuffer :> sig
  type buffer

  exception BufferFull
  exception BufferEmpty

  val create : int -> buffer
  val read : buffer -> int
  val write : buffer -> int -> unit
  val clear : buffer -> unit
  val overwrite : buffer -> int -> unit
end = struct

  type buffer = {
    ary: int array,
    size: int,
    readp: int ref,
    writep: int ref,
    count: int ref
  }

  exception BufferFull
  exception BufferEmpty

  fun create size = {
    ary = Array.array (size, 0),
    size = size,
    readp = ref 0,
    writep = ref 0,
    count = ref 0
  }

  fun read buff =
    if !(#count buff) = 0
    then raise BufferEmpty
    else let val value = Array.sub (#ary buff, !(#readp buff))
         in  ( (#readp buff) := (!(#readp buff) + 1) mod #size buff;
               (#count buff) := !(#count buff) - 1;
               value
             )
         end

  fun write buff value =
    if !(#count buff) = #size buff
    then raise BufferFull
    else ( Array.update (#ary buff, !(#writep buff), value);
           (#writep buff) := (!(#writep buff) + 1) mod #size buff;
           (#count buff)  := !(#count buff) + 1
         )

  fun clear buff = (
    (#writep buff) := !(#readp buff);
    (#count buff)  := 0
  )

  fun overwrite buff value =
    let val _ = if !(#count buff) = #size buff then read buff else 0
    in  write buff value
    end
end
