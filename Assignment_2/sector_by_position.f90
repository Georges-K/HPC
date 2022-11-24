subroutine sector_by_position(X,Y,S)
implicit none

integer :: COL, ROW, i, S(n)
real(8) :: DIS_1, DIS_2, WIDTH, X(n), Y(n)
WIDTH = L/nsec

do i=1, n

	DIS_1 = X(i) + (L/2)
	IF (DIS_1 < L .AND. 0.0 < DIS_1) THEN
		COL = floor(DIS_1/WIDTH)
	ELSE IF (DIS_1 >= L) THEN
		COL = floor(L/WIDTH) - 1
	ELSE IF (DIS_1 < 0.0) THEN
		COL = 0
	END IF
	

	DIS_2 = -Y(i) + (L/2)
	IF (DIS_2 < L .AND. 0.0 < DIS_2) THEN
		ROW = floor(DIS_2/WIDTH)
	ELSE IF (DIS_2 >= L) THEN
		ROW = floor(L/WIDTH) - 1
	ELSE IF (DIS_2 < 0.0) THEN
		ROW = 0
	END IF
	
	
	S(i) = ROW*nsec + COL


end do

return 

end subroutine sector_by_position


