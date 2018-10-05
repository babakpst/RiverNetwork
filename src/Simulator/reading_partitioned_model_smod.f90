
submodule (reading_network_mod) reading_network_submod

implicit none

contains

!##################################################################################################
!##################################################################################################

module procedure reading_partitioned_network

Implicit None

! Local Variables =================================================================================
! - integer Variables -----------------------------------------------------------------------------
integer(kind=Smll) :: ERR_Alloc, ERR_DeAlloc ! Allocating and DeAllocating errors
integer(kind=Smll) :: UnFile                 ! Holds Unit of a file for error message
integer(kind=Smll) :: IO_File                ! For IOSTAT: Input Output status in OPEN command
integer(kind=Smll) :: IO_read                ! Holds error of read statements
integer(kind=Smll) :: IO_write               ! Used for IOSTAT in the write statement:In/Out Status
integer(kind=Smll) :: i_reach                ! loop index on the number of reaches
integer(kind=Lng)  :: i_cells                ! loop index on the number of reaches

! code ============================================================================================
write(*,       *)
write(*,       *) " Subroutine < Input_sub >: "
write(FileInfo,*)
write(FileInfo,*) " Subroutine < Input_sub >: "

! Open required Files -----------------------------------------------------------------------------
write(*,        fmt="(A)") " -Opening the input file ..."
write(FileInfo, fmt="(A)") " -Opening the input file ..."

! Open the input file for arrays
UnFile = FilePartition
open(Unit=UnFile, file=trim(ModelInfo%ModelNameParallel)//'.par', &
     err=1001, iostat=IO_File, access='sequential', action='read', asynchronous='no', &
     blank='null', blocksize=0, defaultfile=trim(ModelInfo%InputDir), DisPOSE='keep', &
     form='formatted', position='asis', status='old')

UnFile = FilePartition
read(unit=UnFile, fmt="(4I23)", advance='yes', asynchronous='no', iostat=IO_read, err=1003, &
     end=1004) &
     this%TotalNumOfCellsOnThisRank, this%TotalNumOfReachesOnThisRank, &
     this%TotalNumOfCellsInTheNetwork, this%TotalNumOfReachesInTheNetwork

write(*,        fmt="(A)") " -Allocating arrays for the discretized network ..."
write(FileInfo, fmt="(A)") " -Allocating arrays for the discretized network ..."

allocate(this%DiscretizedReach( this%TotalNumOfReachesOnThisRank), stat=ERR_Alloc)
if (ERR_Alloc /= 0) call error_in_allocation(ERR_Alloc)

this%NCutsOnRanks = 0_Lng  ! Initializing number of reach cuts

  On_Reaches: do i_reach = 1, this%TotalNumOfCellsOnThisRank

    read(unit=UnFile, fmt="(2I23)", advance='yes', asynchronous='no', iostat=IO_read, err=1003, &
         end=1004) &
         this%DiscretizedReach(i_reach)%ReachNumber,    &!reach number in the unpartitioned network- global reach number
         this%DiscretizedReach(i_reach)%Communication,  &
         this%DiscretizedReach(i_reach)%CommRank,       &
         this%DiscretizedReach(i_reach)%BCNodeI,        &
         this%DiscretizedReach(i_reach)%BCNodeII,       &
         this%DiscretizedReach(i_reach)%NCells_reach,   &

         this%DiscretizedReach(i_reach)%UpstreamReaches(1,1),   &
         this%DiscretizedReach(i_reach)%UpstreamReaches(2,1),   &
         this%DiscretizedReach(i_reach)%DownstreamReaches(1,1),   &

         this%DiscretizedReach(i_reach)%UpstreamReaches(1,2),   &
         this%DiscretizedReach(i_reach)%UpstreamReaches(2,2),   &
         this%DiscretizedReach(i_reach)%DownstreamReaches(1,2),   &

         this%DiscretizedReach(i_reach)%ReachManning,   &
         this%DiscretizedReach(i_reach)%ReachWidthCell, &
         this%DiscretizedReach(i_reach)%CellPorjectionLength

    ! compute the number of reach cuts on this rank-if communication is -1, means that the entire
    ! reach is on one rank, thus, there is no cut. But if communication is not -1, means that
    ! this reach has been cut.
    if (this%DiscretizedReach(i_reach)%Communication /=-1_lng)  &
                                                      this%NCutsOnRanks = this%NCutsOnRanks + 1_Lng

    ! allocating the arrays within each descretized reach
    allocate(                                                                                     &
    this%DiscretizedReach(i_reach)%LengthCell(this%DiscretizedReach(i_reach)%NCells_reach),       &
    this%DiscretizedReach(i_reach)%CellSlope(this%DiscretizedReach(i_reach)%NCells_reach),        &
    this%DiscretizedReach(i_reach)%InterfaceSlope(this%DiscretizedReach(i_reach)%NCells_reach+1), &

    this%DiscretizedReach(i_reach)%ZCell(this%DiscretizedReach(i_reach)%NCells_reach),            &
    this%DiscretizedReach(i_reach)%YCell(this%DiscretizedReach(i_reach)%NCells_reach),            &
    this%DiscretizedReach(i_reach)%XCell(this%DiscretizedReach(i_reach)%NCells_reach),            &
    stat=ERR_Alloc)

    if (ERR_Alloc /= 0) call error_in_allocation(ERR_Alloc)

    UnFile = FilePartition
      do i_cells = 1_Lng, this%DiscretizedReach(i_reach)%NCells_reach
        read(unit=UnFile, fmt="(6F35.20)", advance='yes', asynchronous='no', iostat=IO_read, &
        err=1003, end=1004) &
        this%DiscretizedReach(i_reach)%LengthCell    (i_cells),  &
        this%DiscretizedReach(i_reach)%CellSlope     (i_cells),  &
        this%DiscretizedReach(i_reach)%InterfaceSlope(i_cells),  &
        this%DiscretizedReach(i_reach)%ZCell         (i_cells),  &
        this%DiscretizedReach(i_reach)%YCell         (i_cells),  &
        this%DiscretizedReach(i_reach)%XCell         (i_cells)
      end do
    read(unit=UnFile, fmt="(F35.20)", advance='yes', asynchronous='no', iostat=IO_read, &
         err=1003, end=1004) this%DiscretizedReach(i_reach)%InterfaceSlope(i_cells+1_Lng)
  end do On_Reaches

! - Closing the input file ------------------------------------------------------------------------
write(*,        fmt="(A)") " -Closing the input file"
write(FileInfo, fmt="(A)") " -Closing the input file"

UnFile = FilePartition
close(Unit = UnFile, status = 'keep', ERR =  1002, IOSTAT = IO_File)

write(*,       *) " End Subroutine < Input_sub >"
write(*,       *)
write(FileInfo,*) " End Subroutine < Input_sub >"
write(FileInfo,*)
Return

! Errors ==========================================================================================
! Opening statement Errors
1001 call errorMessage(UnFile, IO_File)

! Close statement Errors
1002 call error_in_closing_a_file(UnFile, IO_File)

! - Error in read statement -----------------------------------------------------------------------
1003 call error_in_reading_data_from_file(UnFile, IO_read)

! - End-OF-FILE in read statement -----------------------------------------------------------------
1004 call error_in_reading_data_from_file_EOF(UnFile, IO_read)

! - write statement error -------------------------------------------------------------------------
1006 call error_in_writing(UnFile, IO_write)

end procedure reading_partitioned_network

end submodule reading_network_submod
