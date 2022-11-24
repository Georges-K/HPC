subroutine sector_to_board (sector_var, M, board_var) 
    ! This routine takes a single integer between 0 and M^2-1 that represents the sector number
    ! and converts it to indexed board coordinates i_row, j_col 

    ! INPUT, the sector number and M [inputs dont get modified]
    integer, intent (in) :: sector_var, M     

    ! Local dummy variables to represent the row/column of a sector
    integer :: i_row, j_col         

    ! OUTPUT, a pair of integers that represent board coordinates [INPUTTED ARRAY VAR MODIFIED]
    integer, intent (out) :: board_var(0:1)

    ! To get the row number, we can divide by M and rounding down, this is akin to row many rows we go down 
    ! [NOTE] Fortran by default does truncating division when dividing two integers, no need for floor function
    i_row = sector_var/M

    ! To get the column number, we just check the remainder value to get the column 
    j_col = modulo(sector_var, M)
    
    ! Finally, we obtain the following 2-vector
    board_var = [i_row, j_col] 
end subroutine sector_to_board 

subroutine board_to_sector (board_var, M, sector_var) 
    ! This routine takes a board coordinate array and returns a scalar integer sector number

    ! INPUT, board coordinate array and M [inputs dont get modified]
    integer, intent(in) :: board_var(0:1), M

    ! OUTPUT, an integer scalar that is the sector number [INPUTTED SCALAR VAR MODIFIED]
    integer, intent(out):: sector_var

    ! We get the sector number by going down by the row number times M and right by the column number
    !print*,board_var(0),board_var(1)
    sector_var = (board_var(0) * M) + board_var(1)
end subroutine board_to_sector

subroutine neighbour_array_generator (neighbour_array) 
    ! This routine generates a precached array of all neighbour sectors for a given box subdivision M
!    external sector_to_board, board_to_sector
    ! INPUT, all we need to know is how long the row is since the box is square and the mesh is static
    integer :: M_tot, cntr

    ! Dummy variables to represent the current sector both as an integer and board coordinate
    integer :: sector, board(0:1) = 0

    ! Dummy variables for neighbouring sectors 
        ! - Neb_row and col are counters that go between -1 and 1 to check adjacency 
        ! - Neb_num is a counter that goes from 0 to 8 that is used to index each of the 9 possible neighbours
        ! - [NOTE] every single sector will always have 9 neighbours, invalid neighbours will be indexed as sector (-1)
        ! - [NOTE] make sure that when reading the neighbour array, sector -1 should be skipped  
    integer :: Neb_sector, Neb_row = 0, Neb_col = 0, Neb_num = 0, Neb_board(0:1)

    ! OUTPUT, a 2-d array with dimensions (0:M_tot, 0:8) 
    ! i.e. number of sectors is the number of rows, and the 9 possible neighbours are the 9 colums) 
    ! [NOTE] MAKE SURE YOU ALLOCATE THE ARRAY AND SET THE CORRECT DIMENSIONS BEFORE CALLING THIS ROUTINE 
    integer, intent(out) :: neighbour_array(0:,0:)

    ! Define M_tot
    M_tot = nsec**2 - 1

    ! We first fill the entire array with (-1) and then later replace only the valid neighbours found
    neighbour_array = -1
!    neighbour_array = -1/0.

    ! start at sector 0, end at final sector
    do sector = 0, M_tot

        ! Start by converting the current sector to board coordinates
        call sector_to_board(sector, nsec, board)
        cntr = 0
            ! Check row above -> Check own row -> Check row below (-1 is above, +1 is below)
            do Neb_row = -1, 1

                ! Neighbour board coordinates are the current board coordinates plus the row shift
                Neb_board(0) = board(0) + Neb_row

                ! Convert Neighbour board to Neighbour sector number [MIGHT BE REDUNDANT, TEST!!!]
                call board_to_sector(Neb_board, nsec, Neb_sector)

                ! Check left column -> Check own column -> Check right column 
                do Neb_col = -1, 1
                    ! Column shift
                    Neb_board(1) = board(1) + Neb_col

                    ! After doing the shifts, we have to check to see if the given neighbour is valid
                    ! if either coordinate is positive and less than M, the neighbour is inside domain
                    if (Neb_board(1) > -1 .AND. Neb_board(1) < nsec .AND. Neb_board(0) > -1 .AND. Neb_board(0) < nsec) then
                        ! Convert valid neighbour to sector number and write to array 
                        call board_to_sector(Neb_board, nsec, Neb_sector)
                        neighbour_array(sector, cntr) = Neb_sector
                        cntr = cntr + 1
                    end if                    

                    ! Move to next neighbour index
                    Neb_num = Neb_num + 1
                end do


            end do
        neighbour_array(sector,9) = cntr
        ! Reset neighbour index to 0
        Neb_num = 0 
    end do

end subroutine neighbour_array_generator
