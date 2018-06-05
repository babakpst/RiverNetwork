
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
! V2.00: 04/16/2018 - Modified for the partitioner.
! V2.20: 05/30/2018 - Initializing types.
!
! File version $Id $
!
! Last update: 05/30/2018
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
use Input_mod, only: Input_Data_tp
use messages_and_errors_mod

implicit none
private

! Plot domain
type Plot_domain_1D_tp(NCells)
  integer(kind=Lng), len :: NCells =0_lng

  real(kind=DBL), dimension(NCells) :: XCoor      ! Location of cell centers
  real(kind=DBL), dimension(NCells) :: ZCoor      ! Bathymetry
  real(kind=DBL), dimension(NCells) :: SlopeCell  ! Slope of the domain at the locaiton of
                                                  ! cell center

  Character(kind = 1, len = 20) :: IndexSize !Size no in the Char. fmt to add to input file Name

  contains
    procedure plot => Plot_Domain_1D_sub
end type Plot_domain_1D_tp

public:: Plot_domain_1D_tp

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
write(*,       *) " -Writing down the domain in the .Domain file ... "
write(FileInfo,*) " -Writing down the domain in the .Domain file ... "

UnFile = FileDomain
open(unit=UnFile, &
     file=trim(ModelInfo%ModelName)//'_s'//trim(adjustL(this%IndexSize))//'.Domain', Err=1001,&
     iostat=IO_File, access='sequential', action='write', asynchronous='no', blank='NULL', &
     blocksize=0, defaultfile=trim(ModelInfo%OutputDir), dispose='keep', form='formatted', &
     position='asis', status='replace')

UnFile = FileDomain
write(unit=UnFile, fmt="(' Domain coordinates: ')", advance='yes', asynchronous='no', &
      iostat=IO_write, err=1006)
write(unit=UnFile, fmt="(' Number of points: ')",   advance='yes', asynchronous='no', &
      iostat=IO_write, err=1006)
write(unit=UnFile, fmt="(I20)",                     advance='yes', asynchronous='no', &
      iostat=IO_write, err=1006) this%NCells
write(unit=UnFile, fmt="(' x   --      z ')",       advance='yes', asynchronous='no', &
      iostat=IO_write, err=1006)

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
1001 call errorMessage(UnFile, IO_File)

! Close statement Errors
1002 call error_in_closing_a_file(UnFile, IO_File)

! write statement errors
1006 call error_in_writing(UnFile, IO_write)

end subroutine Plot_Domain_1D_sub

end module Results_mod
