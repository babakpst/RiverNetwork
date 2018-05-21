
!##################################################################################################
! Purpose: This module writes down the input file for each process/rank.
!
! Developed by: Babak Poursartip
! Supervised by: Clint Dawson
!
! The Institute for Computational Engineering and Sciences (ICES)
! The University of Texas at Austin
!
! ================================ V E R S I O N ==================================================
! V0.00: 04/15/2018 - File initiated.
!
! File version $Id $
!
! Last update: 04/17/2018
!
! ================================ S U B R O U T I N E ============================================
! Partitioner_1D_Sub: Creates the input files for various processes.
!
! ================================ F U N C T I O N ================================================
!
! ================================ L O C A L   V A R I A B L E S ==================================
! (Refer to the main code to see the list of imported variables)
!  . . . . . . . . . . . . . . . . Variables . . . . . . . . . . . . . . . . . . . . . . . . . . .
!
!##################################################################################################

module Partitioner_mod

! Libraries =======================================================================================

! User defined modules ============================================================================
use Parameters_mod
use Discretization_mod

implicit none

contains

!##################################################################################################
! Purpose: This subroutine creates the input file for each partition/process.
!
! Developed by: Babak Poursartip
!
! The Institute for Computational Engineering and Sciences (ICES)
! The University of Texas at Austin
!
! ================================ V E R S I O N ==================================================
! V0.00: 04/15/2018 - Subroutine initiated.
! V0.01: 04/16/2018 - Initiated: Compiled without error for the first time.
!
! File version $Id $
!
! Last update: 04/16/2018
!
! ================================ L O C A L   V A R I A B L E S ==================================
! (Refer to the main code to see the list of imported variables)
!  . . . . . . . . . . . . . . . . Variables . . . . . . . . . . . . . . . . . . . . . . . . . . .
!
!##################################################################################################

subroutine Partitioner_1D_Sub(Geometry, Discretization, ModelInfo)

! Libraries =======================================================================================

! User defined modules ============================================================================

implicit none

! Global variables ================================================================================

! - types -----------------------------------------------------------------------------------------
type(Input_Data_tp),     intent(In) :: ModelInfo ! Holds info. (name, dir, output dir) of the model
type(Geometry_tp),       intent(In) :: Geometry
type(model_tp), intent(In) :: Discretization

! Local variables =================================================================================
! - integer variables -----------------------------------------------------------------------------
integer(kind=Smll) :: UnFile      ! Holds Unit of a file for error message
integer(kind=Smll) :: IO_File        ! For IOSTAT: Input Output Status in OPEN command
integer(kind=Smll) :: IO_write       ! Used for IOSTAT: Input/Output Status in the write command

integer(kind=Shrt) :: i_partition ! Loop index for the number of processes/ranks
integer(kind=Shrt) :: i           ! Loop index
integer(kind=Shrt) :: remainder   !

integer(kind=Lng) :: counter      ! Counter for cells
integer(kind=Lng) :: i_cells      ! Loop index for counting number of cells

! - integer Arrays --------------------------------------------------------------------------------
integer(kind=Lng), dimension (Geometry%size)  :: chunk       ! share of the domain for each rank

! - character variables ---------------------------------------------------------------------------
Character(kind = 1, len = 20) :: IndexRank ! Rank no in the Char. fmt to add to the input file Name

! code ============================================================================================
write(*,       *) " subroutine < Partitioner_1D_Sub >: "
write(FileInfo,*) " subroutine < Partitioner_1D_Sub >: "

! Computing the chunk of each process
remainder = mod(Discretization%NCells, Geometry%size)
chunk(:) = (Discretization%NCells - remainder)/Geometry%size
counter = 0_Lng

  do i = 1_Shrt, remainder
    chunk(i) = chunk(i) + 1
  end do

write(*,       *) " -Data partitioning ... "
write(FileInfo,*) " -Data partitioning ... "

  On_Partitions: do i_partition = 1, Geometry%size
    write(*,*)
    write(*,        fmt="(A,I10)") " Partitioning for process no.: ", i_partition-1_Shrt
    write(FileInfo, fmt="(A,I10)") " Partitioning for process no.: ", i_partition-1_Shrt

    ! Opening the output files.
    write(*,       *) " Creating input files ..."
    write(FileInfo,*) " Creating input files ..."

    ! Directories for the input files <modify>
    ! open file for this partition
    write(IndexRank, *) i_partition - 1_Shrt ! Converts Rank to Character format for the file Name

    UnFile = FilePartition
    open(unit=UnFile, &
         file=trim(ModelInfo%ModelName)//'_s'//&
              trim(adjustL(Geometry%IndexSize))//'_p'//trim(adjustL(IndexRank))//'.par', &
         Err=1001, iostat=IO_File, access='sequential', action='write', asynchronous='no', &
         blank='NULL', blocksize=0, defaultfile=trim(ModelInfo%InputDir), dispose='keep', &
         form='formatted', position='asis', status='replace')

    ! Writing the total number of cells in each partition
    UnFile = FilePartition
    write(unit=UnFile, fmt="(I23)", advance='yes', asynchronous='no', &
          iostat=IO_write, err=1006) chunk(i_partition)

    ! Writing the length of each cell in each partition
    write(unit=UnFile, fmt="(F23.8)", advance='yes', asynchronous='no', &
          iostat=IO_write, err=1006) Discretization%LengthCell(1)

    UnFile = FilePartition

      do i_cells = 1_Lng, chunk(i_partition)

        counter = counter + 1_Lng
        write(unit=UnFile, fmt="(11F35.20)", &
              advance='yes', asynchronous='no', iostat=IO_write, err=1006) &
              Discretization%LengthCell (counter),                         &
              Discretization%SlopeCell  (counter),                         &
              Discretization%ZCell      (counter),                         &
              Discretization%ManningCell(counter),                         &
              Discretization%WidthCell  (counter),                         &
              Discretization%X_Disc     (counter),                         &
              Discretization%SlopeInter (counter),                         &
              Discretization%ZFull      (counter*2_Lng-1_Lng),             &
              Discretization%ZFull      (counter*2_Lng),                   &
              Discretization%X_Full     (counter*2_Lng-1_Lng),             &
              Discretization%X_Full     (counter*2_Lng      )
      end do
    write(unit=UnFile, fmt="(F35.20)", &
          advance='yes', asynchronous='no', iostat=IO_write, err=1006) &
          Discretization%SlopeInter(counter+1_Lng)

    ! - Closing the input file for this partition -------------------------------------------------
    write(*,        *) " Closing the input file ... "
    write(FileInfo, *) " Closing the input file ... "

    UnFile =  FilePartition
    close(unit=UnFile, status="keep", err=1002, iostat=IO_File)

  end do On_Partitions

  if (counter/= Discretization%NCells) then
    write(*,       *) "Fatal error! Mismatch in the number of cells. &
                       Check the partitioner subroutine.", counter, Discretization%NCells
    write(FileInfo,*) "Fatal error! Mismatch in the number of cells. &
                       Check the partitioner subroutine.", counter, Discretization%NCells
    write(*, Fmt_FL); write(FileInfo, Fmt_FL);
    write(*, Fmt_end); read(*,*); stop;
  end if

write(*,       *) " Partitioning conducted successfully."
write(FileInfo,*) " Partitioning conducted successfully."

write(*,       *) " end subroutine < Partitioner_1D_Sub >"
write(FileInfo,*) " end subroutine < Partitioner_1D_Sub >"
return

! Errors ==========================================================================================
! Opening statement Errors
1001 if (IO_File > 0) then
       write(*, Fmt_Err1_OPEN) UnFile, IO_File; write(FileInfo, Fmt_Err1_OPEN) UnFile, IO_File;
       write(*, Fmt_FL); write(FileInfo, Fmt_FL);
       write(*, Fmt_end); read(*,*); stop;
     else if ( IO_File < 0 ) then
       write(*, Fmt_Err1_OPEN) UnFile, IO_File
       write(FileInfo, Fmt_Err1_OPEN) UnFile, IO_File;  write(*, Fmt_FL) ; write(FileInfo, Fmt_FL);
       write(*, Fmt_end); read(*,*); stop;
     end if

! Close statement Errors
1002 if (IO_File > 0) then
       write(*, Fmt_Err1_Close) UnFile, IO_File; write(FileInfo, Fmt_Err1_Close) UnFile, IO_File;
       write(*, Fmt_FL); write(FileInfo, Fmt_FL);
       write(*, Fmt_End); read(*,*); stop;
     end if

! write statement errors
1006 write(*, Fmt_write1) UnFile, IO_write; write(UnFile, Fmt_write1) UnFile, IO_write;
     write(*, Fmt_FL); write(FileInfo, Fmt_FL); write(*, Fmt_End); read(*,*);  stop;


end subroutine Partitioner_1D_Sub

end module Partitioner_mod
