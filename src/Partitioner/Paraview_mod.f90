

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


module Paraview_mod

! Libraries =======================================================================================

! User defined modules ============================================================================
use Parameters_mod
use Model_mod, only: Geometry_tp
use Discretize_the_network_mod, only: DiscretizedNetwork_tp
use messages_and_errors_mod
use hdf5

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
type NetworkGeometry_tp(nReaches)

  integer(kind=Lng), len :: nReaches

  ! contains all the coordinates of all reaches of the network
  type(GeometryReach_tp), dimension(nReaches)  :: NetworkGeometry

  contains
    procedure Calc_Geometry  => paraview_Geometry_sub
    procedure Write_Geometry => paraview_HDF5_sub
end type NetworkGeometry_tp

public:: NetworkGeometry_tp

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
! V0.01: 01/28/2019 - Compiled successfully for the first time.
!
! File version $Id $
!
! Last update: 01/28/2019
!
!##################################################################################################

subroutine paraview_Geometry_sub (this, Geometry, Discretization)

! Libraries =======================================================================================

! User defined modules ============================================================================

implicit none

! Global variables ================================================================================

! - types -----------------------------------------------------------------------------------------
type(Geometry_tp), intent(in)   :: Geometry    ! Holds information about the geometry of the domain
type(DiscretizedNetwork_tp), intent(in) :: Discretization ! Discretization

class(NetworkGeometry_tp(nReaches=*) ), intent(inout) :: this

! Local variables =================================================================================
! - integer variables -----------------------------------------------------------------------------
integer(kind=Lng) :: i_Reach       ! loop index for reach numbers
integer(kind=Lng) :: No_CellsReach ! no. of cells in the each reach
integer(kind=Lng) :: i_cells       ! loop index for cell numbers
integer(kind=Lng) :: Node_I        ! node number of the reach
integer(kind=Lng) :: Node_II       ! node number of the reach
integer(kind=Lng) :: nDivision     ! no of cells in each reach

integer(kind=Smll) :: ERR_Alloc, ERR_DeAlloc ! Allocating and DeAllocating errors

! - real variables --------------------------------------------------------------------------------
real(kind=Dbl)    :: x1, y1, x2, y2 ! coordinates of the two nodes of the reach

real(kind=Dbl)    :: cell_length_x ! the length of the cell in each reach in the x dir
real(kind=Dbl)    :: cell_length_y ! the length of the cell in each reach in the y dir


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

    ! Calculating the coordinates of the cells in each reach

    ! finding the nodes of the reach
    Node_I  = Geometry%network(i_Reach)%ReachNodes(1)   ! upstream node
    Node_II = Geometry%network(i_Reach)%ReachNodes(2)   ! downstream node

    ! Coordinates of the nodes
    x1 = Geometry%NodeCoor(Node_I, 1)
    y1 = Geometry%NodeCoor(Node_I, 2)

    x2 = Geometry%NodeCoor(Node_II, 1)
    y2 = Geometry%NodeCoor(Node_II, 2)

    ! finding the number of divisions of the reach
    nDivision = Geometry%network(i_Reach)%NCells_Reach

    ! calculating the horizontal and vertical coordinates of the cell centers
    cell_length_x = (x2 - x1) / nDivision
    cell_length_y = (y2 - y1) / nDivision

      ! loop on the cells
      Do i_cells = 1, nDivision
        this%NetworkGeometry(i_reach)%XCell(i_cells) = x1 + cell_length_x * (i_cells - 0.5)
        this%NetworkGeometry(i_reach)%YCell(i_cells) = y1 + cell_length_y * (i_cells - 0.5)
      end do

    ! We already calculated the height of cell centers in the discretization module.
    this%NetworkGeometry(i_reach)%ZCell(:) = Discretization%DiscretizedReach(i_reach)%ZCell(:)

  end do


write(*,        fmt="(' Discretizing the network was successful. ')")
write(FileInfo, fmt="(' Discretizing the network was successful. ')")

write(*,       *) " end subroutine < paraview_Geometry_sub >"
write(FileInfo,*) " end subroutine < paraview_Geometry_sub >"

write(*,       *)
write(FileInfo,*)

return
end subroutine paraview_Geometry_sub

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

subroutine paraview_HDF5_sub (this, Geometry, Discretization, NetworkPartitioner)

! Libraries =======================================================================================

! User defined modules ============================================================================

implicit none

! Global variables ================================================================================

! - types -----------------------------------------------------------------------------------------
type(Geometry_tp), intent(in)   :: Geometry    ! Holds information about the geometry of the domain
type(DiscretizedNetwork_tp), intent(In) :: Discretization ! Holds the discretized network
type(partitioner_tp(edges=*, nodes=*)), intent(in)    :: NetworkPartitioner
class(NetworkGeometry_tp(nReaches=*) ), intent(inout) :: this

! Local variables =================================================================================
! - integer variables -----------------------------------------------------------------------------
integer(kind=Lng)  :: i_rank        ! loop index on the number of partitions
integer(kind=Lng)  :: RankNodeI     ! Temp var to hold the rank no. of the firs node of each reach
integer(kind=Lng)  :: RankNodeII    ! Temp var to hold the rank no. of the firs node of each reach

! - real variables --------------------------------------------------------------------------------
real(kind=Dbl), dimension(:,:), allocatable :: dset_data_real ! Data buffers

! - character variables ---------------------------------------------------------------------------
character(kind = 1, len=3   ), parameter :: geo = "Geo"
Character(kind = 1, len = 20 ):: IndexReach !Reach no in the Char. fmt to add to input file Name
Character(kind = 1, len = 20 ):: IndexRank  !Rank no in the Char. fmt to add to input file Name

! - HDF5 variables --------------------------------------------------------------------------------
integer(HID_T) :: id_Geometry      ! the geometry h5 file

integer(HID_T) :: dset_id_XYZ      ! data set for coordinates
integer(HID_T) :: dset_id_CNN      ! data set for node connectivity

integer(HID_T) :: dspace_id_XYZ    ! data space for coordinate
integer(HID_T) :: dspace_id_CNN    ! data space for connectivity

integer(HSIZE_T), dimension(2) :: dims  ! data set dimensions

! - type ------------------------------------------------------------------------------------------

! code ============================================================================================
write(*,       *) " subroutine < paraview_HDF5_sub >: "
write(FileInfo,*) " subroutine < paraview_HDF5_sub >: "

write(*,       *) " -..."
write(FileInfo,*) " -..."

! creating the HDF5 files

call h5open_f(error)


  do i_reach = 1, Geometry%Base_Geometry%NoReaches


    ! The rank which the first node of this reach belongs to
    RankNodeI  = NetworkPartitioner%ReachPartition(i_reach,1)

    ! The rank which the second node of this reach belongs to
    RankNodeII = NetworkPartitioner%ReachPartition(i_reach,2)

      ! Check whether the entire reach belongs to one rank or two
      if ( RankNodeI == RankNodeII ) then
        ! the entire reach belongs to one reach

        RangeCell_I   = 1_Lng
        RangeCell_II  = NetworkPartitioner%ReachPartition(i_reach,3)


      else if ( RankNodeI == RankNodeII ) then
        ! this each is shared btw two ranks


      end if


    ! converting the rank number to character
    write(IndexRank, *) i_rank  ! converts rank to Character format for the file Name
    write(IndexReach,*) i_reach ! converts reach no. to Chr format for the file Name


    ! Initialize the dset_data array
    call h5fcreate_f(trim(geo)//'_Rank_'// trim(adjustL(IndexSize))// &
                     '_Reach_'//trim(adjustL(IndexRank))//'.h5', &
                     H5F_ACC_TRUNC_F, id_Geometry, error) ! Geometry file for the network

    ! Create the dataspaces
    ! Coordinate file for the main code(.XYZ)
    dims(1) = 3   ! n dimension
    dims(2) = 5   ! no of cells in each reach

    call h5screate_simple_f(rank, dims, dspace_id_XYZ_1, error)
    call h5dcreate_f(id_Geometry_1, "XYZ",  H5T_NATIVE_DOUBLE,  dspace_id_XYZ_1, dset_id_XYZ_1, error)

    write (*,*)"writing coordinates ..."

    allocate (dset_data_real( dims(1), dims(2) ) )




  end do






















write(*,        fmt="(' Creating the geometry files for Paraview was successful. ')")
write(FileInfo, fmt="(' Creating the geometry files for Paraview was successful. ')")

write(*,       *) " end subroutine < paraview_HDF5_sub >"
write(FileInfo,*) " end subroutine < paraview_HDF5_sub >"

write(*,       *)
write(FileInfo,*)

return
end subroutine paraview_HDF5_sub




end module paraview_mod


