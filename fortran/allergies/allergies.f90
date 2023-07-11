
module allergies
  implicit none

  private
  public allergicTo, allergicList

  character(12), parameter :: allergens(8) = &
    [ "eggs        ", "peanuts     ", "shellfish   ", "strawberries", &
      "tomatoes    ", "chocolate   ", "pollen      ", "cats        "  ]

contains

  function allergicList(allergy_key)
    integer, intent(in) :: allergy_key
    character(len=100) :: allergicList
    integer :: i

    allergicList = ''
    do i = 1, 8
      if (btest(allergy_key, i - 1)) then
        allergicList = trim(allergicList) // ' ' // trim(allergens(i))
      end if
    end do
    allergicList = adjustl(allergicList)
  end function allergicList

  logical function allergicTo(allergy_str, allergy_key)
    character(len=*), intent(in) :: allergy_str
    integer, intent(in) :: allergy_key

    allergicTo = 0 /= index(allergicList(allergy_key), allergy_str)
  end function allergicTo

end module
