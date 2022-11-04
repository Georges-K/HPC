subroutine matrix_mul(n,A,B,C)
  implicit none
  integer :: n,i,j,k
  real(8) :: A(n,n),B(n,n),C(n,n)
  C = 0d0
  do i=1,n
     do j=1,n
        do k=1,n
           C(i,j) = C(i,j) + A(i,k)*B(k,j)
        end do
     end do
  end do

  return
end subroutine matrix_mul
