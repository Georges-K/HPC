subroutine particle_list(X,Y, secNumber, p_list)
   ! Input: X is an array of length n, Y is an array of length n, M is an integer (squares in a row), L is a real number (length of box)
   ! S is an integer (number of sector), n is an integer (number of particles)
   ! Output: p_list is an array of n length with entries -1 for particles outside of the sector

implicit none
integer :: secNumber, i, counter, S(n) 
real(8) :: X(n), Y(n)
integer :: p_list(:)

call sector_by_position(X, Y, S)

counter = 1
do i = 1, n 
   if (S(i) == secNumber) then 
         ! Add that particle to the list
         p_list(counter) = i
         counter = counter + 1
   else 
      p_list(i) = -1 
   end if 
end do
p_list(n+1) = counter - 1

!   print *, p_list
end subroutine particle_list
