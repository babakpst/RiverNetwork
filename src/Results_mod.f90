
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
!
! File version $Id $
!
! Last update: 03/01/2018
!
! ================================ S U B R O U T I N E ============================================
! 1D_Domain: Plot the discretized domain.
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

implicit none

!public
!private


! Plot domain
type, :: Plot_domain_1D_tp(NPoints)
  integer(kind=Lng), len :: NPoints

  character (kind = 1, Len = 30 ) :: ModelName     ! Name of the model input file
  character (kind = 1, Len = 150) :: AnalysisDir   ! Directory of Analysis input file.
  character (kind = 1, Len = 150) :: AnalysisName  ! Name of the file
  character (kind = 1, Len = 150) :: OutputDir     ! Directory of output files (Results)
  character (kind = 1, Len = 150) :: AnalysisOutputDir! Directory of output file for each analysis

  real(kind=DBL), dimension(NPoints) :: XCoor  ! Horizontal points
  real(kind=DBL), dimension(NPoints) :: ZCoor  ! Horizontal points
  contains
    procedure plot => Plot_Domain_1D
end type Plot_domain_1D_tp

private :: Plot_Domain_1D

  interface Results
    module procedure Plot_Domain_1D
  end interface


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

subroutine Plot_Domain_1D(                                            &
!                                                                     & ! integer (1) variables
!                                                                     & ! integer (2) variables
!                                                                     & ! integer (4) variables
!                                                                     & ! integer (8) variables
!                                                                     & ! real variables
!                                                                     & ! integer arrays
!                                                                     & ! real arrays
!                                                                     & ! characters
this                                                                  & ! type
)


! Libraries =======================================================================================

! User defined modules ============================================================================


implicit none

! Global variables ================================================================================

! - types -----------------------------------------------------------------------------------------
class(Plot_domain_1D_tp) :: this


! Local variables =================================================================================
! - integer variables -----------------------------------------------------------------------------
integer(kind=Smll) :: UnFile          ! Holds Unit of a file for error message
integer(kind=Smll) :: IO_File         ! For IOSTAT: Input Output Status in OPEN command
integer(kind=Smll) :: IO_write       ! Used for IOSTAT: Input/Output Status in the write command

integer(kind=Lng)  :: i_point              ! Loop index on the points

! - real variables --------------------------------------------------------------------------------
!#real (kind=Dbl)      ::
! - complex variables -----------------------------------------------------------------------------
!#complex              ::
! - integer Arrays --------------------------------------------------------------------------------
!#integer (kind=Shrt), dimension (:)  ::
!#integer (kind=Shrt), Allocatable, dimension (:)  ::
! - real Arrays -----------------------------------------------------------------------------------
!#real (kind=Dbl), dimension (:)      ::
!#real (kind=Dbl), allocatable, dimension (:)  ::
! - character variables ---------------------------------------------------------------------------
!#character (kind = ?, Len = ? ) ::
! - logical variables -----------------------------------------------------------------------------
!#logical   ::
! - type ------------------------------------------------------------------------------------------

! code ============================================================================================
write(*,       *) " subroutine < Plot_Domain_1D >: "
write(FileInfo,*) " subroutine < Plot_Domain_1D >: "

! - Opening the domain file -----------------------------------------------------------------------
UnFile = FileDomain
open(unit=UnFile, file=trim(this%ModelName)//'.Domain', Err=1001, iostat=IO_File, &
access='sequential', action='write', asynchronous='no', blank='NULL', blocksize=0, defaultfile=trim(this%AnalysisDir), &
dispose='keep', form='formatted', position='asis', status='replace')

UnFile = FileDomain
write(unit=UnFile, fmt="("Domain coordinates: ")", advance='yes', asynchronous='no', iostat=IO_write, err=1006)
write(unit=UnFile, fmt="(" x   --      z ")", advance='yes', asynchronous='no', iostat=IO_write, err=1006)

  do i_points = 1_Lng, this%NPoints
    write(unit=UnFile, fmt="(2F20.5)", advance='yes', asynchronous='no', iostat=IO_write, err=1006) this%XCoor(i_point), this%ZCoor(i_point)
  end do

! - Closing the domain file -----------------------------------------------------------------------
UnFile =  FileDomain
close(unit=UnFile, status="keep", err=1002, iostat=IO_File)


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

write(*,       *) " end subroutine < Plot_Domain_1D >"
write(FileInfo,*) " end subroutine < Plot_Domain_1D >"
return
end subroutine Plot_Domain_1D

end module Results_mod
