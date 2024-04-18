public function my_find(sequence values, atom value)
  integer i = 1, j = length(values)
  while i <= j do
    integer mid = floor((i + j) / 2)
    switch compare(value, values[mid]) do
      case -1 then j = mid - 1
      case  0 then return mid
      case  1 then i = mid + 1
    end switch
  end while
  return -1
end function
