

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
use Parameters_mod

implicit none

private
type ResultReach_tp

  ! contains the solution of this reach, size: no. of cells on this reach
  ! the first element U(1) is height and the second term is U(2) is uh
  type(vector), allocatable, dimension(:) :: U
end type ResultReach_tp

type ResultNetwork_tp

  ! holds the time step
  integer(kind=Lng) :: step

  ! contains all the coordinates of all reaches of the network, size: no. of Reaches on this rank
  type(ResultReach_tp), allocatable, dimension(:)  :: ResultReach

  contains
    procedure Wrapper   => Wrapper_File_Creator_sub ! wrapper file
    procedure ReachFile => Reach_File_Creator_sub   ! local xdmf file
    procedure Results   => Result_File_Creator_sub  ! hdf5 creator
end type ResultNetwork_tp

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

subroutine Result_File_Creator_sub(this)

! Libraries =======================================================================================

! User defined modules ============================================================================

implicit none

! Global variables ================================================================================

! - types -----------------------------------------------------------------------------------------
class(ResultNetwork_tp) :: this

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
