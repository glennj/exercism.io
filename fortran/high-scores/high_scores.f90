
module high_scores
  implicit none
contains

function scores(scoreList)
  integer, dimension(:), intent(in) :: scoreList
  integer :: scores(size(scoreList))
  scores(:) = scoreList(:)
end function

integer function latest(scoreList)
  integer, dimension(:), intent(in) :: scoreList
  latest = scoreList(size(scoreList))
end function

integer function personalBest(scoreList)
  integer, dimension(:), intent(in) :: scoreList
  integer :: i

  personalBest = scoreList(1)
  do i = 2, size(scoreList)
    if (scoreList(i) > personalBest) then
      personalBest = scoreList(i)
    end if
  end do
end function

function personalTopThree(scoreList) result(top3)
  integer, dimension(:), intent(in) :: scoreList
  integer, dimension(3) :: top3
  integer :: sorted(size(scoreList)), i, j, tmp

  ! naive sorting algorithm
  sorted(:) = scoreList(:)
  do i = 1, size(sorted) - 1
    do j = i + 1, size(sorted)
      if (sorted(i) < sorted(j)) then
        tmp = sorted(i)
        sorted(i) = sorted(j)
        sorted(j) = tmp
      end if
    end do
  end do

  if (size(sorted) < 3) then
    top3 = [sorted(1), 0, 0]
    if (size(sorted) == 2) top3(2) = sorted(2)
  else
    top3 = sorted(1:3)
  end if
end function
  
end module
