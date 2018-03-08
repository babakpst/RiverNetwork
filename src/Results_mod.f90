
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
type Plot_domain_1D_tp(NCells)
  integer(kind=Lng), len :: NCells

  real(kind=DBL), dimension(NCells) :: XCoor  ! Horizontal points
  real(kind=DBL), dimension(NCells) :: ZCoor  ! Horizontal points
  real(kind=DBL), dimension(NCells) :: SlopeCell  ! Horizontal points

  contains
    procedure plot => Plot_Domain_1D_sub
end type Plot_domain_1D_tp


type Plot_Results_1D_tp(NCells)
  integer(kind=Lng), len :: NCells
  real(kind=dbl), dimension(NCells)    :: h
  real(kind=dbl), dimension(NCells)    :: uh
  real(kind=dbl), dimension(NCells)    :: s_f
  real(kind=dbl), dimension(NCells)    :: s
  real(kind=dbl), dimension(NCells-1)  :: hm
  real(kind=dbl), dimension(NCells-1)  :: uhm
  real(kind=dbl), dimension(NCells-1)  :: s_f_m
  real(kind=dbl), dimension(NCells-1)  :: s_m

  type(Input_Data_tp) :: ModelInfo

  contains
    procedure plot_results => Plot_Results_1D_sub
end type Plot_Results_1D_tp



private :: Plot_Domain_1D_sub, Plot_Results_1D_sub

  interface Results
    module procedure Plot_Domain_1D_sub
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

subroutine Plot_Domain_1D_sub(                                        &
!                                                                     & ! integer (1) variables
!                                                                     & ! integer (2) variables
!                                                                     & ! integer (4) variables
!                                                                     & ! integer (8) variables
!                                                                     & ! real variables
!                                                                     & ! integer arrays
!                                                                     & ! real arrays
!                                                                     & ! characters
this,ModelInfo                                                        & ! type
)


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
type(Input_Data_tp), intent(in) :: ModelInfo  ! Holds info. (name, dir, output dir) of the model

! code ============================================================================================
write(*,       *) " subroutine < Plot_Domain_1D_sub >: "
write(FileInfo,*) " subroutine < Plot_Domain_1D_sub >: "

! - Opening the domain file -----------------------------------------------------------------------
UnFile = FileDomain

write(*,       *) " -Writing down the domain in the .Domain file ... "
write(FileInfo,*) " -Writing down the domain in the .Domain file ... "

open(unit=UnFile, file=trim(ModelInfo%ModelName)//'.Domain', Err=1001, iostat=IO_File, &
access='sequential', action='write', asynchronous='no', blank='NULL', blocksize=0, defaultfile=trim(ModelInfo%OutputDir), &
dispose='keep', form='formatted', position='asis', status='replace')

UnFile = FileDomain
write(unit=UnFile, fmt="(' Domain coordinates: ')", advance='yes', asynchronous='no', iostat=IO_write, err=1006)
write(unit=UnFile, fmt="(' Number of points: ')", advance='yes', asynchronous='no', iostat=IO_write, err=1006)
write(unit=UnFile, fmt="(I20)", advance='yes', asynchronous='no', iostat=IO_write, err=1006) this%NCells
write(unit=UnFile, fmt="(' x   --      z ')", advance='yes', asynchronous='no', iostat=IO_write, err=1006)

  do i_points = 1_Lng, this%NCells
    write(unit=UnFile, fmt="(i16,3F16.5)", advance='yes', asynchronous='no', iostat=IO_write, err=1006) i_points, this%XCoor(i_points), this%ZCoor(i_points), this%SlopeCell(i_points)
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
! Purpose: This subroutine plots the result of
!
! Developed by: Babak Poursartip
! Supervised by: Clint Dawson
!
! The Institute for Computational Engineering and Sciences (ICES)
! The University of Texas at Austin
!
! ================================ V E R S I O N ==================================================
! V0.00: 03/05/2018 - File initiated.
! V0.01: 03/05/2018 - Initiated: Compiled without error for the first time.
!
! File version $Id $
!
! Last update: 03/05/2018
!
! ================================ L O C A L   V A R I A B L E S ==================================
! (Refer to the main code to see the list of imported variables)
!  . . . . . . . . . . . . . . . . Variables . . . . . . . . . . . . . . . . . . . . . . . . . . .
!
!##################################################################################################

subroutine Plot_Results_1D_sub(                                       &
!                                                                     & ! integer (1) variables
!                                                                     & ! integer (2) variables
!                                                                     & ! integer (4) variables
!                                                                     & ! integer (8) variables
!                                                                     & ! real variables
!                                                                     & ! integer arrays
!                                                                     & ! real arrays
!                                                                     & ! characters
this, i_step                                                          & ! type
)

! Libraries =======================================================================================


! User defined modules ============================================================================



implicit none

! Global variables ================================================================================

! - integer variables -----------------------------------------------------------------------------
integer(kind=Lng), intent(in) :: i_step

! - real variables --------------------------------------------------------------------------------
!#real (kind=Dbl),     intent(in)    ::
!#real (kind=Dbl),     intent(inout) ::
!#real (kind=Dbl),     intent(out)   ::
! - complex variables -----------------------------------------------------------------------------
!#complex,             intent(in)    ::
!#complex,             intent(inout) ::
!#complex,             intent(out)   ::
! - integer Arrays --------------------------------------------------------------------------------
!#integer (kind=Shrt), intent(in),    dimension (:  )  ::
!#integer (kind=Shrt), intent(in),    dimension (:,:)  ::
!#integer (kind=Shrt), intent(in)    ::
!#integer (kind=Shrt), intent(inout) ::
!#integer (kind=Shrt), intent(out)   ::
! - real Arrays -----------------------------------------------------------------------------------
!#real (kind=Dbl),     intent(in),    dimension (:  )  ::
!#real (kind=Dbl),     intent(inout), dimension (:  )  ::
!#real (kind=Dbl),     intent(out),   dimension (:  )  ::
! - character variables ---------------------------------------------------------------------------
!#character (kind = ?, Len = ? ) ::
! - logical variables -----------------------------------------------------------------------------
!#logical   ::
! - types -----------------------------------------------------------------------------------------
class(Plot_Results_1D_tp(*)) :: this

! Local variables =================================================================================
! - integer variables -----------------------------------------------------------------------------
integer(kind=Smll) :: UnFile         ! Holds Unit of a file for error message
integer(kind=Smll) :: IO_File        ! For IOSTAT: Input Output Status in OPEN command
integer(kind=Smll) :: IO_write       ! Used for IOSTAT: Input/Output Status in the write command

integer(kind=Lng)  :: i_points       ! Loop index on the points

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
character (kind = 1, Len = 30) :: extfile

! - logical variables -----------------------------------------------------------------------------
!#logical   ::
! - type ------------------------------------------------------------------------------------------

! code ============================================================================================
write(*,       *) " subroutine < Plot_Results_1D_sub >: "
write(FileInfo,*) " subroutine < Plot_Results_1D_sub >: "

! - Opening the domain file -----------------------------------------------------------------------
UnFile = FileResults

write(*,       *) " -Writing down the results in the .Res file ... "
write(FileInfo,*) " -Writing down the results in the .Res file ... "

write (extfile,*) i_step

open(unit=UnFile, file=trim(this%ModelInfo%ModelName)//'_'//trim(ADJUSTL(extfile))//'.Res', Err=1001, iostat=IO_File, &
access='sequential', action='write', asynchronous='no', blank='NULL', blocksize=0, defaultfile=trim(this%ModelInfo%AnalysisOutputDir), &
dispose='keep', form='formatted', position='asis', status='replace')

UnFile = FileResults
!write(unit=UnFile, fmt="(' Results: ')", advance='yes', asynchronous='no', iostat=IO_write, err=1006)
!write(unit=UnFile, fmt="(' Number of points: ')", advance='yes', asynchronous='no', iostat=IO_write, err=1006)
!write(unit=UnFile, fmt="(I20)", advance='yes', asynchronous='no', iostat=IO_write, err=1006) this%NCells
!write(unit=UnFile, fmt="(' h      --      uh ')", advance='yes', asynchronous='no', iostat=IO_write, err=1006)

  do i_points = 1_Lng, this%NCells
    write(unit=UnFile, fmt="(I6, 8F16.5)", advance='yes', asynchronous='no', iostat=IO_write, err=1006) i_points, this%h(i_points), this%uh(i_points), this%s_f(i_points), this%s(i_points), this%hm(i_points), this%uhm(i_points), this%s_f_m(i_points), this%s_m(i_points)
  end do
  !write(unit=UnFile, fmt="(I6, 8F16.5)", advance='yes', asynchronous='no', iostat=IO_write, err=1006) this%h(i_points), this%uh(i_points), this%s_f(i_points), this%s(i_points)


write(*,        fmt = "(A,I10)") " Results was written successfully in the file for time step: ", i_step
write(FileInfo, fmt = "(A,I10)") " Results was written successfully in the file for time step: ", i_step

! - Closing the domain file -----------------------------------------------------------------------
UnFile =  FileResults
close(unit=UnFile, status="keep", err=1002, iostat=IO_File)

write(*,       *) " end subroutine < Plot_Results_1D_sub >"
write(FileInfo,*) " end subroutine < Plot_Results_1D_sub >"
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

end subroutine Plot_Results_1D_sub



end module Results_mod
