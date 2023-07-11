module reverse_string
  implicit none
contains

  function reverse(input) result(reversed)
    character(*), intent(in) :: input
    character(len=len(input)) :: reversed
    integer :: i, j
    
    i = 1
    j = len(input)

    do while (i <= j)
      reversed(j:j) = input(i:i)
      reversed(i:i) = input(j:j)
      i = i + 1
      j = j - 1
    end do
  end function
end module
