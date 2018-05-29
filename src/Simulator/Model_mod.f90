
!##################################################################################################
! Purpose: This module reads all data for simulation.
!
! Developed by: Babak Poursartip
! Supervised by: Clint Dawson
!
! The Institute for Computational Engineering and Sciences (ICES)
! The University of Texas at Austin
!
! ================================ V E R S I O N ==================================================
! V0.10: 02/22/2018 - Initiation.
! V0.10: 03/08/2018 - Initiated: Compiled without error.
! V1.00: 04/20/2018 - Major modifications
! V2.00: 05/15/2018 - Separating model from the input
!
! File version $Id $
!
! Last update: 05/15/2018
!
! ================================ S U B R O U T I N E ============================================
! Input_Address_sub: Reads file name and directories from the address file.
! Input_Array_sub
! Input_Analysis_sub
!
! ================================ F U N C T I O N ================================================
!
! ================================ L O C A L   V A R I A B L E S ==================================
! (Refer to the main code to see the list of imported variables)
!  . . . . . . . . . . . . . . . . Variables . . . . . . . . . . . . . . . . . . . . . . . . . . .
!
!##################################################################################################

module Model_mod

use Parameters_mod
use Input_mod

implicit none
private

! Contains all information after discretization
type model_tp
  integer (kind=Lng)  :: NCells ! Total number of cells in the domain

  real(kind=DBL), allocatable, dimension(:) :: SlopeCell  ! the slope of each cell at the center
  real(kind=DBL), allocatable, dimension(:) :: SlopeInter ! the slope of each cell at the center
  real(kind=DBL), allocatable, dimension(:) :: ZCell      ! bottom elev. at the center of each cell

  real(kind=DBL), allocatable, dimension(:) :: ManningCell! the Manning's number of each cell
  real(kind=DBL), allocatable, dimension(:) :: WidthCell  ! the Manning's number of each cell
  real(kind=DBL), allocatable, dimension(:) :: X_Disc     ! the coordinates of the cell center
  real(kind=DBL), allocatable, dimension(:,:) :: LengthCell ! the length of each cell
            ! note: the first col holds the actual cell length (length of the control volume), and
            !       the second col holds the projection(x)

  !real(kind=DBL), allocatable, dimension(:) :: ZFull      ! bottom elevation at all points
  !real(kind=DBL), allocatable, dimension(:) :: X_Full     ! the coordinates all points
  contains
    procedure:: Input => Input_sub

end type model_tp

public:: model_tp

contains

!##################################################################################################
! Purpose: This subroutine reads the partitioned data created by the partitioner.
!
! Developed by: Babak Poursartip
! Supervised by: Clint Dawson
!
! The Institute for Computational Engineering and Sciences (ICES)
! The University of Texas at Austin
!
! ================================ V E R S I O N ==================================================
! V0.1: 02/23/2018 - Initiation.
! V1.0: 03/01/2018 - Compiled with no errors/warnings.
! V1.0: 04/10/2018 - Minor modifications in the class.
! V2.0: 04/20/2018 - Parallel.
! V2.1: 05/24/2018 - Parallel with MPI
!
! File version $Id $
!
! Last update: 05/24/2018
!
! ================================ L O C A L   V A R I A B L E S ==================================
! (Refer to the main code to see the list of imported variables)
!  . . . . . . . . . . . . . . . . Variables . . . . . . . . . . . . . . . . . . . . . . . . . . .
!
!##################################################################################################

Subroutine Input_sub(this, ModelInfo)

! Libraries =======================================================================================

! User defined modules ============================================================================

Implicit None

! Global Variables ================================================================================

! - types -----------------------------------------------------------------------------------------
type(Input_Data_tp), intent(In)    :: ModelInfo  ! Holds info. (name, dir, output dir) of the model
class(model_tp),  intent(inout) :: this ! Holds the model

! Local Variables =================================================================================
! - integer Variables -----------------------------------------------------------------------------
integer(kind=Smll) :: ERR_Alloc, ERR_DeAlloc ! Allocating and DeAllocating errors
integer(kind=Smll) :: UnFile   ! Holds Unit of a file for error message
integer(kind=Smll) :: IO_File  ! For IOSTAT: Input Output status in OPEN command
integer(kind=Smll) :: IO_read  ! Holds error of read statements
integer(kind=Smll) :: IO_write ! Used for IOSTAT - Input Output Status - in the write command
integer(kind=Smll) :: i_reach  ! loop index on the number of reaches

integer(kind=Lng)  :: i_cells  ! loop index on the number of reaches

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

open(Unit=UnFile, file=trim(ModelInfo%ModelName)//'.par', &
     err=1001, iostat=IO_File, access='sequential', action='read', asynchronous='no', &
     blank='null', blocksize=0, defaultfile=trim(ModelInfo%InputDir), DisPOSE='keep', &
     form='formatted', position='asis', status='old')

UnFile = FilePartition
read(unit=UnFile, fmt="(I23)", advance='yes', asynchronous='no', iostat=IO_read, err=1003, &
     end=1004) this%NCells

write(*,        fmt="(A)") " -Allocating discretization ..."
write(FileInfo, fmt="(A)") " -Allocating discretization ..."

allocate(this%LengthCell(this%NCells,2),            &
         this%SlopeCell(this%NCells),               &
         this%SlopeInter(this%NCells+1),            &
         this%ZCell(this%NCells),                   &
         this%ManningCell(this%NCells),             &
         this%WidthCell(this%NCells),               &
         this%X_Disc(this%NCells),                  &
         stat=ERR_Alloc)
         !this%X_Full(this%NCells*2_Lng + 1_Lng),    &
         !this%ZFull(this%NCells*2_Lng + 1_Lng),     &

  if (ERR_Alloc /= 0) then
    write (*, Fmt_ALLCT) ERR_Alloc;  write (FileInfo, Fmt_ALLCT) ERR_Alloc;
    write(*, Fmt_FL);  write(FileInfo, Fmt_FL); read(*, Fmt_End);  stop;
  end if

UnFile = FilePartition
  do i_cells = 1_Lng, this%NCells
    read(unit=UnFile, fmt="(8F35.20)", advance='yes', asynchronous='no', iostat=IO_read, &
    err=1003, end=1004) &
    this%LengthCell (i_cells,1),                       &
    this%LengthCell (i_cells,2),                       &
    this%SlopeCell  (i_cells),                         &
    this%ZCell      (i_cells),                         &
    this%ManningCell(i_cells),                         &
    this%WidthCell  (i_cells),                         &
    this%X_Disc     (i_cells),                         &
    this%SlopeInter (i_cells)

    !this%ZFull      (i_cells*2_Lng-1_Lng),             &
    !this%ZFull      (i_cells*2_Lng),                   &
    !this%X_Full     (i_cells*2_Lng-1_Lng),             &
    !this%X_Full     (i_cells*2_Lng      )
  end do
!read(unit=UnFile, fmt="(F35.20)", advance='yes', asynchronous='no', iostat=IO_read, &
!    err=1003, end=1004) this%SlopeInter(i_cells)

read(unit=UnFile, fmt="(F35.20)", advance='yes', asynchronous='no', iostat=IO_read, &
    err=1003, end=1004) this%SlopeInter(this%NCells+1)

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
1001 if (IO_File > 0) then
       write(*, Fmt_Err1_OPEN) UnFile, IO_File; write(FileInfo, Fmt_Err1_OPEN) UnFile, IO_File;
       write(*, Fmt_FL); write(FileInfo, Fmt_FL);
       write(*, Fmt_End); read(*,*); stop;
     Else if ( IO_File < 0 ) then
       write(*, Fmt_Err1_OPEN) UnFile, IO_File
       write(FileInfo, Fmt_Err1_OPEN) UnFile, IO_File;  write(*, Fmt_FL) ; write(FileInfo, Fmt_FL);
       write(*, Fmt_End); read(*,*); stop;
     end if


! Close statement Errors
1002 if (IO_File > 0) then
       write(*, Fmt_Err1_Close) UnFile, IO_File; write(FileInfo, Fmt_Err1_Close) UnFile, IO_File;
       write(*, Fmt_FL); write(FileInfo, Fmt_FL);
       write(*, Fmt_End); read(*,*); stop;
     end if

! - Error in read statement -----------------------------------------------------------------------
1003 write(*, Fmt_read1) UnFile, IO_read; write(UnFile, Fmt_read1) UnFile, IO_read;
     write(*, Fmt_FL);  write(FileInfo, Fmt_FL); write(*, Fmt_End); read(*,*);  stop;

! - End-OF-FILE in read statement -----------------------------------------------------------------
1004 write(*, Fmt_read2) UnFile, IO_read; write(UnFile, Fmt_read2) UnFile, IO_read;
     write(*, Fmt_FL);  write(FileInfo, Fmt_FL); write(*, Fmt_End); read(*,*);  stop;

! - End-OF-FILE IN read statement -----------------------------------------------------------------
1005 write(*, Fmt_read3) UnFile, IO_read; write(UnFile, Fmt_read3) UnFile, IO_read;
     write(*, Fmt_FL);  write(FileInfo, Fmt_FL); write(*, Fmt_End); read(*,*);  stop;

! - write statement error -------------------------------------------------------------------------
1006 write(*, Fmt_write1) UnFile, IO_write; write(UnFile, Fmt_write1) UnFile, IO_write;
     write(*, Fmt_FL); write(FileInfo, Fmt_FL); write(*, Fmt_End); read(*,*);  stop;

end Subroutine Input_sub

end module Model_mod
