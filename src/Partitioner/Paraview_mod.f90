

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
!
! File version $Id $
!
! Last update: 01/24/2019
!
! ================================ S U B R O U T I N E ============================================
!
!
! ================================ F U N C T I O N ================================================
!
!
!##################################################################################################


module paraview_mod

! Libraries =======================================================================================

! User defined modules ============================================================================
use Parameters_mod


implicit none
private

!
type



  contains

end type





public::

contains

!##################################################################################################
! Purpose: This subroutine discretizes the geometry of the network  and creates geometry files.
!
! Developed by: Babak Poursartip
! Supervised by: Clint Dawson
!
! The Institute for Computational Engineering and Sciences (ICES)
! The University of Texas at Austin
!
! ================================ V E R S I O N ==================================================
! V0.00: 01/24/2019 - File initiated.
!
! File version $Id $
!
! Last update: 01/24/2019
!
!##################################################################################################

subroutine paraview_Geometry_sub (this)

! Libraries =======================================================================================

! User defined modules ============================================================================

implicit none

! Global variables ================================================================================

! - types -----------------------------------------------------------------------------------------


class(  ), intent(inout) :: this

! Local variables =================================================================================
! - integer variables -----------------------------------------------------------------------------
integer(kind=Smll) ::



! - real variables --------------------------------------------------------------------------------
real(kind=Dbl)    ::
! - type ------------------------------------------------------------------------------------------


! code ============================================================================================
write(*,       *) " subroutine < paraview_Geometry_sub >: "
write(FileInfo,*) " subroutine < paraview_Geometry_sub >: "

write(*,       *) " -..."
write(FileInfo,*) " -..."








write(*,        fmt="(' Discretizing the network was successful. ')")
write(FileInfo, fmt="(' Discretizing the network was successful. ')")

write(*,       *) " end subroutine < paraview_Geometry_sub >"
write(FileInfo,*) " end subroutine < paraview_Geometry_sub >"

write(*,       *)
write(FileInfo,*)

return
end subroutine paraview_sub














!##################################################################################################
! Purpose: This subroutine discretizes the geometry of the network  and creates geometry files.
!
! Developed by: Babak Poursartip
! Supervised by: Clint Dawson
!
! The Institute for Computational Engineering and Sciences (ICES)
! The University of Texas at Austin
!
! ================================ V E R S I O N ==================================================
! V0.00: 01/24/2019 - File initiated.
!
! File version $Id $
!
! Last update: 01/24/2019
!
!##################################################################################################

subroutine paraview_HDF5_sub (this)

! Libraries =======================================================================================

! User defined modules ============================================================================

implicit none

! Global variables ================================================================================

! - types -----------------------------------------------------------------------------------------


class(  ), intent(inout) :: this

! Local variables =================================================================================
! - integer variables -----------------------------------------------------------------------------
integer(kind=Smll) ::



! - real variables --------------------------------------------------------------------------------
real(kind=Dbl)    ::
! - type ------------------------------------------------------------------------------------------


! code ============================================================================================
write(*,       *) " subroutine < paraview_HDF5_sub >: "
write(FileInfo,*) " subroutine < paraview_HDF5_sub >: "

write(*,       *) " -..."
write(FileInfo,*) " -..."






write(*,        fmt="(' Creating the geometry files for Paraview was successful. ')")
write(FileInfo, fmt="(' Creating the geometry files for Paraview was successful. ')")

write(*,       *) " end subroutine < paraview_HDF5_sub >"
write(FileInfo,*) " end subroutine < paraview_HDF5_sub >"

write(*,       *)
write(FileInfo,*)

return
end subroutine paraview_HDF5_sub




end module paraview_mod

