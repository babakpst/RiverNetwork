
!##################################################################################################
! Purpose: This module writes down or visualizes the results.
!
! Developed by: Babak Poursartip
! Supervised by: Clint Dawson
!
! The Institute for Computational Engineering and Sciences (ICES)
! The University of Texas at Austin
!
! ================================ V E R S I O N ==================================================
! V0.00: 02/26/2018 - File initiated.
! V0.01: 03/02/2018 - Initiated: Compiled without error for the first time.
! V0.10: 03/08/2018 - Initiated: Compiled without error.
! V1.00: 03/20/2018 - Compiled without error.
! V2.00: 04/15/2018 - Modified for the partitioner.
!
! File version $Id $
!
! Last update: 04/16/2018
!
! ================================ S U B R O U T I N E ============================================
! 1D_Domain: Plot the discretized domain.
! Partitioner_1D_Sub: Creates the input files for various processes.
!
! ================================ F U N C T I O N ================================================
!
! ================================ L O C A L   V A R I A B L E S ==================================
! (Refer to the main code to see the list of imported variables)
!  . . . . . . . . . . . . . . . . Variables . . . . . . . . . . . . . . . . . . . . . . . . . . .
!
!##################################################################################################

module Results_mod

! Libraries =======================================================================================


! User defined modules ============================================================================
use Parameters_mod
use Input_mod

implicit none

!public
!private

! This vector will be used in the main type as the solution in each type step
type vector
  real(kind=Dbl), dimension(2) :: U
end type vector

! Plot domain
type Plot_domain_1D_tp(NCells)
  integer(kind=Lng), len :: NCells

  real(kind=DBL), dimension(NCells) :: XCoor  ! Horizontal points
  real(kind=DBL), dimension(NCells) :: ZCoor  ! Horizontal points
  real(kind=DBL), dimension(NCells) :: SlopeCell  ! Horizontal points

  contains
    procedure plot => Plot_Domain_1D_sub
end type Plot_domain_1D_tp





type partition_tp

  contains
   procedure partition => Partitioner_1D_Sub

end type partition_tp






contains


!##################################################################################################
! Purpose: This subroutine plots the 1D domain
!
! Developed by: Babak Poursartip
!
! The Institute for Computational Engineering and Sciences (ICES)
! The University of Texas at Austin
!
! ================================ V E R S I O N ==================================================
! V0.00: 03/01/2018 - File initiated.
! V0.01: 03/01/2018 - Initiated: Compiled without error for the first time.
!
! File version $Id $
!
! Last update: 03/01/2018
!
! ================================ L O C A L   V A R I A B L E S ==================================
! (Refer to the main code to see the list of imported variables)
!  . . . . . . . . . . . . . . . . Variables . . . . . . . . . . . . . . . . . . . . . . . . . . .
!
!##################################################################################################

subroutine Plot_Domain_1D_sub(this,ModelInfo)


! Libraries =======================================================================================

! User defined modules ============================================================================


implicit none

! Global variables ================================================================================

! - types -----------------------------------------------------------------------------------------
class(Plot_domain_1D_tp(*)) :: this


! Local variables =================================================================================
! - integer variables -----------------------------------------------------------------------------
integer(kind=Smll) :: UnFile         ! Holds Unit of a file for error message
integer(kind=Smll) :: IO_File        ! For IOSTAT: Input Output Status in OPEN command
integer(kind=Smll) :: IO_write       ! Used for IOSTAT: Input/Output Status in the write command

integer(kind=Lng)  :: i_points       ! Loop index on the points

! - type ------------------------------------------------------------------------------------------
type(Input_Data_tp), intent(in) :: ModelInfo  ! Holds info. (name, dir, output dir) of the model

! code ============================================================================================
write(*,       *) " subroutine < Plot_Domain_1D_sub >: "
write(FileInfo,*) " subroutine < Plot_Domain_1D_sub >: "

! - Opening the domain file -----------------------------------------------------------------------
UnFile = FileDomain

write(*,       *) " -Writing down the domain in the .Domain file ... "
write(FileInfo,*) " -Writing down the domain in the .Domain file ... "

open(unit=UnFile, file=trim(ModelInfo%ModelName)//'.Domain', Err=1001, iostat=IO_File, &
     access='sequential', action='write', asynchronous='no', blank='NULL', blocksize=0, &
     defaultfile=trim(ModelInfo%OutputDir), dispose='keep', form='formatted', position='asis', &
     status='replace')

UnFile = FileDomain
write(unit=UnFile, fmt="(' Domain coordinates: ')", advance='yes', asynchronous='no', iostat=IO_write, err=1006)
write(unit=UnFile, fmt="(' Number of points: ')",   advance='yes', asynchronous='no', iostat=IO_write, err=1006)
write(unit=UnFile, fmt="(I20)",                     advance='yes', asynchronous='no', iostat=IO_write, err=1006) this%NCells
write(unit=UnFile, fmt="(' x   --      z ')",       advance='yes', asynchronous='no', iostat=IO_write, err=1006)

  do i_points = 1_Lng, this%NCells
    write(unit=UnFile, fmt="(i16,3F16.5)", advance='yes', asynchronous='no', iostat=IO_write, &
          err=1006) i_points, this%XCoor(i_points), this%ZCoor(i_points), this%SlopeCell(i_points)
  end do

write(*,       *) " Domain coordinates was written successfully in the file. "
write(FileInfo,*) " Domain coordinates was written successfully in the file. "

! - Closing the domain file -----------------------------------------------------------------------
UnFile =  FileDomain
close(unit=UnFile, status="keep", err=1002, iostat=IO_File)

write(*,       *) " end subroutine < Plot_Domain_1D_sub >"
write(FileInfo,*) " end subroutine < Plot_Domain_1D_sub >"
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

end subroutine Plot_Domain_1D_sub


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

subroutine Partitioner_1D_Sub(this, Geometry)

! Libraries =======================================================================================

! User defined modules ============================================================================

implicit none

! Global variables ================================================================================

! - integer variables -----------------------------------------------------------------------------
!#integer(kind=Shrt), intent(in)    ::
!#integer(kind=Shrt), intent(inout) ::
!#integer(kind=Shrt), intent(out)   ::
! - real variables --------------------------------------------------------------------------------
!#real(kind=Dbl),     intent(in)    ::
!#real(kind=Dbl),     intent(inout) ::
!#real(kind=Dbl),     intent(out)   ::
! - complex variables -----------------------------------------------------------------------------
!#complex,             intent(in)    ::
!#complex,             intent(inout) ::
!#complex,             intent(out)   ::
! - integer Arrays --------------------------------------------------------------------------------
!#integer(kind=Shrt), intent(in),    dimension (:  )  ::
!#integer(kind=Shrt), intent(in),    dimension (:,:)  ::
!#integer(kind=Shrt), intent(in)    ::
!#integer(kind=Shrt), intent(inout) ::
!#integer(kind=Shrt), intent(out)   ::
! - real Arrays -----------------------------------------------------------------------------------
!#real(kind=Dbl),     intent(in),    dimension (:  )  ::
!#real(kind=Dbl),     intent(inout), dimension (:  )  ::
!#real(kind=Dbl),     intent(out),   dimension (:  )  ::
! - character variables ---------------------------------------------------------------------------
!#character(kind = ?, Len = ? ) ::
! - logical variables -----------------------------------------------------------------------------
!#logical   ::
! - types -----------------------------------------------------------------------------------------
class(partition_tp) :: this
type(Geometry_tp)   :: Geometry


! Local variables =================================================================================
! - integer variables -----------------------------------------------------------------------------
integer(kind=Shrt) :: i_partition ! Loop index for the number of processes/ranks

! - real variables --------------------------------------------------------------------------------
!#real(kind=Dbl)      ::
! - complex variables -----------------------------------------------------------------------------
!#complex              ::
! - integer Arrays --------------------------------------------------------------------------------
!#integer(kind=Shrt), dimension (:)  ::
!#integer(kind=Shrt), Allocatable, dimension (:)  ::
! - real Arrays -----------------------------------------------------------------------------------
!#real(kind=Dbl), dimension (:)      ::
!#real(kind=Dbl), allocatable, dimension (:)  ::
! - character variables ---------------------------------------------------------------------------
!#character(kind = ?, Len = ? ) ::
! - logical variables -----------------------------------------------------------------------------
!#logical   ::
! - type ------------------------------------------------------------------------------------------

! code ============================================================================================
write(*,       *) " subroutine < Partitioner_1D_Sub >: "
write(FileInfo,*) " subroutine < Partitioner_1D_Sub >: "

! Computing the chunk of each process


  On_Partitions: do i_partition = 1, Geometry%size
    write(*,       *) " Creating input files for process no.: ", i_partition-1_Shrt
    write(FileInfo,*) " Creating input files for process no.: ", i_partition-1_Shrt





  end do

write(*,       *) " Partitioning conducted successfully."
write(FileInfo,*) " Partitioning conducted successfully."

write(*,       *) " end subroutine < Partitioner_1D_Sub >"
write(FileInfo,*) " end subroutine < Partitioner_1D_Sub >"
return
end subroutine Partitioner_1D_Sub








end module Results_mod
