

!##################################################################################################
! Purpose: This module provides the input geometry files for visualization with Paraview.
!
! Developed by: Babak Poursartip
! Supervised by: Clint Dawson
!
! The Institute for Computational Engineering and Sciences (ICES)
! The University of Texas at Austin
!
! ================================ V E R S I O N ==================================================
! V0.00: 01/24/2019 - Start the module.
! V0.01: 01/28/2019 - compiled successfully for the first time.
!
! File version $Id $
!
! Last update: 01/28/2019
!
! ================================ S U B R O U T I N E ============================================
!
!
! ================================ F U N C T I O N ================================================
!
!
!##################################################################################################


module ParaviewOutput_mod

! Libraries =======================================================================================
use hdf5

! User defined modules ============================================================================
use messages_and_errors_mod

implicit none

private
type ResultReach_tp
  real(kind=DBL), allocatable, dimenison(:) :: height
  real(kind=DBL), allocatable, dimenison(:) :: velocity
end type ResultReach_tp

type ResultNetwork_tp(nReaches)

  integer(kind=Lng), len :: nReaches

  ! contains all the coordinates of all reaches of the network
  type(ResultReach_tp), dimension(nReaches)  :: ResultReach

  contains
    procedure Wrapper   => Wrapper_File_Creator_sub
    procedure ReachFile => Reach_File_Creator_sub
    procedure Results   => Result_File_Creator_sub
end type

public:: ResultNetwork_tp

contains

!##################################################################################################
! Purpose: This subroutine creates the wrapper file for paraview that contains all reaches in all
!          ranks for all time steps.
!
! Developed by: Babak Poursartip
! Supervised by: Clint Dawson
!
! The Institute for Computational Engineering and Sciences (ICES)
! The University of Texas at Austin
!
! ================================ V E R S I O N ==================================================
! V0.00: 02/11/2019 - File initiated.
! V0.01: 02/11/2019 - Compiled successfully for the first time.
!
! File version $Id $
!
! Last update: 02/11/2019
!
!##################################################################################################

subroutine Wrapper_File_Creator_sub()

end subroutine Wrapper_File_Creator_sub

!##################################################################################################
! Purpose: This subroutine creates xdmf file for each reach in each rank for each time step.
!
! Developed by: Babak Poursartip
! Supervised by: Clint Dawson
!
! The Institute for Computational Engineering and Sciences (ICES)
! The University of Texas at Austin
!
! ================================ V E R S I O N ==================================================
! V0.00: 02/11/2019 - File initiated.
! V0.01: 02/11/2019 - Compiled successfully for the first time.
!
! File version $Id $
!
! Last update: 02/11/2019
!
!##################################################################################################

subroutine Reach_File_Creator_sub()

end subroutine Reach_File_Creator_sub

!##################################################################################################
! Purpose: This subroutine creates hdf5, result file  (velocity and height) for each reach in each
!          rank for each time step.
!
! Developed by: Babak Poursartip
! Supervised by: Clint Dawson
!
! The Institute for Computational Engineering and Sciences (ICES)
! The University of Texas at Austin
!
! ================================ V E R S I O N ==================================================
! V0.00: 02/11/2019 - File initiated.
! V0.01: 02/11/2019 - Compiled successfully for the first time.
!
! File version $Id $
!
! Last update: 02/11/2019
!
!##################################################################################################

subroutine Result_File_Creator_sub()

! Libraries =======================================================================================

! User defined modules ============================================================================

implicit none

! Global variables ================================================================================

! - types -----------------------------------------------------------------------------------------

! Local variables =================================================================================

! code ============================================================================================
!write(*,       *) " subroutine < Result_File_Creator_sub >: "
!write(FileInfo,*) " subroutine < Result_File_Creator_sub >: "







!write(*,       *) " end subroutine < Result_File_Creator_sub >"
!write(FileInfo,*) " end subroutine < Result_File_Creator_sub >"

write(*,       *)
write(FileInfo,*)

return

end subroutine Result_File_Creator_sub


end module ParaviewOutput_mod
