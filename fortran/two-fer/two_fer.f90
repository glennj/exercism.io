module two_fer
  implicit none

contains

  function twoFer(name) result(phrase)
    character(*), intent(in), optional :: name
    character(:), allocatable :: phrase
    character(:), allocatable :: who

    if (present(name)) then
      who = name
    else
      who = "you"
    end if
    phrase = "One for " // who // ", one for me."
  end function twoFer

end module two_fer
