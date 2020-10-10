proc slices*(input: string, size: int): seq[string] =
  if  size <= 0 or input.len == 0 or size > input.len:
    raise newException(ValueError, "bad input")

  for i in size .. input.len:
    result.add input[i - size ..< i] 
