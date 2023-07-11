
module armstrong_numbers
  implicit none
contains

  integer function armstrongSum(n)
    integer, intent(in) :: n
    integer :: num, length

    length = ceiling(log10(real(n)))
    armstrongSum = 0
    num = n

    do while (num > 0)
      armstrongSum = armstrongSum + mod(num, 10) ** length
      num = num / 10
    end do
  end function

  logical function isArmstrongNumber(n)
    integer, intent(in) :: n
    
    isArmstrongNumber = n == armstrongSum(n)
  end function

end module
