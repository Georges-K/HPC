subroutine fillMatrix(M,n)
  ! Subroutine to fill the leading n rows and columns of array M.
  implicit none
  integer :: n,seed_size
  integer, allocatable :: seed(:)
  real(8) :: M(n,n)
  ! Find out how many seeds the default PRNG algorithm needs:
  call random_seed(size = seed_size)
  ! Allocate the seed variable:
  allocate(seed(seed_size))
  ! Somewhat randomly set the seeds and report them:
  call random_seed(get=seed)
  print *,'Running PRNG with ',seed_size,' seeds:'
  print *, seed
  ! Now fill the array with PRNs:
  call random_number(M)
  ! De-allocate the seed variable:
  deallocate(seed)
end subroutine fillMatrix
