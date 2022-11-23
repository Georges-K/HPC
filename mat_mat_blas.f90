program mat_mat
    implicit none
    integer, parameter :: n=1000
    integer :: i,j,k
    real(8) :: A(n,n), B(n,n), C(n,n), start, finish
  
    ! Fill A and B with random numbers on (0,1):
    call fillMatrix(A,n)
    call fillMatrix(B,n)
  
    call cpu_time(start)
    call DGEMM('N','N',n,n,n,1d0,A,n,B,n,0d0,C,n)
    call cpu_time(finish)
  !endtime = = omp_get_wtime()
  print *,'Time elapsed ',finish-start

end program mat_mat