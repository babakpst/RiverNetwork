

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
! Last update: 01/25/2019
!
! ================================ S U B R O U T I N E ============================================
!
!
! ================================ F U N C T I O N ================================================
!
!
!##################################################################################################


module Paraview_mod

! Libraries =======================================================================================

! User defined modules ============================================================================
use Parameters_mod


implicit none
private

! Coordinates corresponding to each reach of the network
type GeometryReach_tp

  real(kind=DBL), allocatable, dimension(:) :: ZCell !bottom elev. at the center of each cell
  real(kind=DBL), allocatable, dimension(:) :: YCell !coordinates of the cell center for each reach
  real(kind=DBL), allocatable, dimension(:) :: XCell !coordinates of the cell center for each reach

  contains

end type GeometryReach_tp

! The class that holds all the coordinated in the entire network for each reach
type NetworGeometry_tp(nReaches)

  integer(kind=Lng), len :: nReaches

  ! contains all the coordinates of all reaches of the network
  type(GeometryReach_tp), dimension(nReaches)  :: NetworkGeometry

  contains
    procedure Calc_Geometry => paraview_Geometry_sub
    procedure
end type NetworGeometry_tp

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
! Last update: 01/25/2019
!
!##################################################################################################

subroutine paraview_Geometry_sub (this, Geometry)

! Libraries =======================================================================================

! User defined modules ============================================================================

implicit none

! Global variables ================================================================================

! - types -----------------------------------------------------------------------------------------
type(Geometry_tp)   :: Geometry       ! Holds information about the geometry of the domain

class( NetworGeometry_tp(nReaches=*) ), intent(inout) :: this

! Local variables =================================================================================
! - integer variables -----------------------------------------------------------------------------
integer(kind=Lng) :: i_Reach ! loop index for reach numbers
integer(kind=Lng) :: No_CellsReach ! no. of cells in the each reach


! - real variables --------------------------------------------------------------------------------
real(kind=Dbl)    ::
! - type ------------------------------------------------------------------------------------------


! code ============================================================================================
write(*,       *) " subroutine < paraview_Geometry_sub >: "
write(FileInfo,*) " subroutine < paraview_Geometry_sub >: "

write(*,       *) " -..."
write(FileInfo,*) " -..."


! allocating the items for each reach.
  do i_reach= 1, Geometry%Base_Geometry%NoReaches

    No_CellsReach = Geometry%network(i_reach)%NCells_Reach

    allocate(                                           &
    this%NetworkGeometry(i_reach)%ZCell(No_CellsReach), &
    this%NetworkGeometry(i_reach)%YCell(No_CellsReach), &
    this%NetworkGeometry(i_reach)%XCell(No_CellsReach), &
    stat=ERR_Alloc)
    if (ERR_Alloc /= 0) call error_in_allocation(ERR_Alloc)

  end do

! Calculating the coordinates of the cells in each reach
  do i_Reach = 1, nReaches





  end do


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
! Last update: 01/25/2019
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

