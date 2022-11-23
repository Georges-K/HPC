  ! Code for multiplying two matrices.
program mat_mat
    use omp_lib
    implicit none
    integer, parameter :: n=1000
    integer :: i,j,k
    real(8) :: A(n,n), B(n,n), C(n,n), start, finish
  
    ! Fill A and B with random numbers on (0,1):
    call fillMatrix(A,n)
    call fillMatrix(B,n)
  
    !call cpu_time(start)
    !start = omp_get_wtime()
    C = 0d0
    !$omp do private(i,j,k)
    do i=1,n
       do j=1,n
          do k=1,n
             C(i,k) = C(i,k) + A(i,j)*B(j,k)
          end do
       end do
    end do
    !$omp end do
    !call cpu_time(finish)
    !finish  = omp_get_wtime()
    !print *,'Time elapsed ',finish-start
    !open(1, file = 'data1.dat', status = 'new')
    !close(1) 
    
  end program mat_mat
  