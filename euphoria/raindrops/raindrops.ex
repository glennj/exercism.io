public function raindrops( atom n )
  sequence sound = ""

  if remainder(n, 3) = 0 then
    sound &= "Pling"
  end if

  if remainder(n, 5) = 0 then
    sound &= "Plang"
  end if

  if remainder(n, 7) = 0 then
    sound &= "Plong"
  end if

  if length(sound) = 0 then
    sound = sprintf("%d", n)
  end if

  return sound
end function
