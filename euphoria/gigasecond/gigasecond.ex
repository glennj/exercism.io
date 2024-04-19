include std/datetime.e

public function add_gigasecond(datetime moment)
  return datetime:add(moment, 1_000_000_000, datetime:SECONDS)
end function
