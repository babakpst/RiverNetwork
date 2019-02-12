
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
! Last update: 02/11/2019
!
! ================================ S U B R O U T I N E ============================================
! 1D_Domain: Plot the discretized domain.
! Plot_Results_1D_sub : Plots the results of the 1D SWE solved by the Lax-Wendroff method.
! Plot_Results_1D_limiter_sub : Plots the results of the 1D SWE solved by the Lax-Wendroff
!                               method with limiter.
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
use messages_and_errors_mod

implicit none
private

! This vector will be used in the main type as the solution in each type step
!type vector_results
!  real(kind=Dbl), dimension(2) :: U
!end type vector_results

!type Plot_Results_1D_limiter_tp(NCells)
!  integer(kind=Lng), len :: NCells

!  type(vector),  dimension(NCells) :: s   ! temp to hold bathymetry

!  type(vector),  dimension(NCells*2) :: phi   ! Holds the value of the limiter function <delete>
!  type(vector),  dimension(NCells*2) :: theta ! Holds the value of the limiter function <delete>

!  type(vector), dimension(NCells)  :: U     ! This vector holds the solution at previous step,
                                            ! the first term holds "h" and the second holds "uh"
!  type(Input_Data_tp) :: ModelInfo

!  contains
!    procedure plot_results => Plot_Results_1D_limiter_sub
!end type Plot_Results_1D_limiter_tp


type Plot_Results_1D_limiter_tp
  integer(kind=Lng) :: NCells
  integer(kind=Lng) :: reach
  integer(kind=Lng) :: step

  type(vector), dimension(:), allocatable :: U  ! This vector holds the solution at previous step,
                                            ! the first term holds "h" and the second holds "uh"
  type(Input_Data_tp) :: ModelInfo

  contains
    procedure plot_results => Plot_Results_1D_limiter_sub
end type Plot_Results_1D_limiter_tp

public:: Plot_Results_1D_limiter_tp

contains


!##################################################################################################
! Purpose: This subroutine plots the results of 1D shallow water equation using the Lax-Wendroff
!          method with limiter.
!
! Developed by: Babak Poursartip
! Supervised by: Clint Dawson
!
! The Institute for Computational Engineering and Sciences (ICES)
! The University of Texas at Austin
!
! ================================ V E R S I O N ==================================================
! V0.00: 03/15/2018 - File initiated.
! V0.01: 03/15/2018 - Initiated: Compiled without error for the first time.
! V2.01: 03/15/2018 - For parallel MPI
! V2.10: 05/31/2018 - Python input file
!
! File version $Id $
!
! Last update: 05/31/2018
!
! ================================ L O C A L   V A R I A B L E S ==================================
! (Refer to the main code to see the list of imported variables)
!  . . . . . . . . . . . . . . . . Variables . . . . . . . . . . . . . . . . . . . . . . . . . . .
!
!##################################################################################################

subroutine Plot_Results_1D_limiter_sub(this)

! Libraries =======================================================================================


! User defined modules ============================================================================

implicit none

! Global variables ================================================================================

! - integer variables -----------------------------------------------------------------------------

! - types -----------------------------------------------------------------------------------------
class(Plot_Results_1D_limiter_tp) :: this

! Local variables =================================================================================
! - integer variables -----------------------------------------------------------------------------
integer(kind=Smll) :: UnFile         ! Holds Unit of a file for error message
integer(kind=Smll) :: IO_File        ! For IOSTAT: Input Output Status in OPEN command
integer(kind=Smll) :: IO_write       ! Used for IOSTAT: Input/Output Status in the write command

integer(kind=Lng)  :: i_points       ! Loop index on the points

! - character variables ---------------------------------------------------------------------------
character (kind = 1, Len = 30) :: reachfile
character (kind = 1, Len = 30) :: extfile

! code ============================================================================================
!write(*,       *) " subroutine < Plot_Results_1D_limiter_sub >: "
!write(FileInfo,*) " subroutine < Plot_Results_1D_limiter_sub >: "

! - Opening the domain file -----------------------------------------------------------------------
UnFile = FileResults

!write(*,       *) " -Writing down the results in the .Res file ... "
!write(FileInfo,*) " -Writing down the results in the .Res file ... "

write (reachfile,*) this%reach
write (extfile,*) this%step

open(unit=UnFile, file=trim(this%ModelInfo%ModelNameParallel)//'_R'&
     //trim(ADJUSTL(reachfile))//'_S'//trim(ADJUSTL(extfile))//'.Res', &
  err=1001, iostat=IO_File, access='sequential', action='write', asynchronous='no', blank='NULL', &
blocksize=0, defaultfile=trim(this%ModelInfo%AnalysisOutputDir), dispose='keep', form='formatted',&
position='asis', status='replace')

UnFile = FileResults
write(unit=UnFile,fmt="(' Results: ')",advance='yes',asynchronous='no', iostat=IO_write, err=1006)
write(unit=UnFile, fmt="(' Number of points: ')", advance='yes', asynchronous='no', &
                                                                        iostat=IO_write, err=1006)
write(unit=UnFile,fmt="(I20)",advance='yes',asynchronous='no',iostat=IO_write,err=1006)this%NCells
write(unit=UnFile,fmt="(' h--uh ')", advance='yes', asynchronous='no', iostat=IO_write, err=1006)

  !do i_points = -1_Lng, this%NCells+2
  do i_points = -1_Lng, this%NCells+2
    write(unit=UnFile, fmt="(I6, 2F30.15)", advance='yes', asynchronous='no', &
     iostat=IO_write, err=1006) i_points, this%U(i_points)%U(1), this%U(i_points)%U(2)
  end do

!write(*,fmt = "(A,I10)") " Results was written successfully in the file for time step: ", i_step
!write(FileInfo,fmt="(A,I10)")" Results was written successfully in the file for time step:",i_step

! - Closing the domain file -----------------------------------------------------------------------
UnFile =  FileResults
close(unit=UnFile, status="keep", err=1002, iostat=IO_File)

!write(*,       *) " end subroutine < Plot_Results_1D_limiter_sub >"
!write(FileInfo,*) " end subroutine < Plot_Results_1D_limiter_sub >"
return

! Errors ==========================================================================================
! Opening statement Errors
1001 call errorMessage(UnFile, IO_File)

! Close statement Errors
1002 call error_in_closing_a_file(UnFile, IO_File)

! write statement errors
1006 call error_in_writing(UnFile, IO_write)

end subroutine Plot_Results_1D_limiter_sub


end module Results_mod
