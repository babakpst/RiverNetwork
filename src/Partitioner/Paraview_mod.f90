

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
! V0.01: 02/10/2019 - compiled successfully for the first time.
!
! File version $Id $
!
! Last update: 02/10/2019
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
use hdf5

! User defined modules ============================================================================
use Parameters_mod
use Model_mod, only: Geometry_tp
use Discretize_the_network_mod, only: DiscretizedNetwork_tp
use messages_and_errors_mod

use Network_Partitioner_mod
use hdf5

implicit none
private

! Coordinates corresponding to each reach of the network
type GeometryReach_tp

  integer(kind=Lng) :: nCells  ! No. of cells in this reach  !<delete>

  real(kind=DBL), allocatable, dimension(:) :: XCell !coordinates of the cell center for each reach
  real(kind=DBL), allocatable, dimension(:) :: YCell !coordinates of the cell center for each reach
  real(kind=DBL), allocatable, dimension(:) :: ZCell !bottom elev. at the center of each cell

  contains

end type GeometryReach_tp

! The class that holds all the coordinated in the entire network for each reach
type NetworkGeometry_tp

  ! contains all the coordinates of all reaches of the network
  type(GeometryReach_tp), allocatable, dimension(:) :: GeometryReach

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

class(NetworkGeometry_tp), intent(inout) :: this

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

! allocating the items for each reach.
  do i_reach= 1, Geometry%Base_Geometry%NoReaches

    No_CellsReach = Geometry%network(i_reach)%NCells_Reach

    allocate(                                         &
    this%GeometryReach(i_reach)%ZCell(No_CellsReach), &
    this%GeometryReach(i_reach)%YCell(No_CellsReach), &
    this%GeometryReach(i_reach)%XCell(No_CellsReach), &
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
        this%GeometryReach(i_reach)%XCell(i_cells) = x1 + cell_length_x * (i_cells - 0.5)
        this%GeometryReach(i_reach)%YCell(i_cells) = y1 + cell_length_y * (i_cells - 0.5)
      end do

    ! We already calculated the height of cell centers in the discretization module.
    this%GeometryReach(i_reach)%ZCell(:) = Discretization%DiscretizedReach(i_reach)%ZCell(:)

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
! V0.01: 02/10/2019 - Compiled successfully for the first time.
!
! File version $Id $
!
! Last update: 02/10/2019
!
!##################################################################################################

subroutine paraview_HDF5_sub (this, Geometry, Discretization, NetworkPartitioner, ModelInfo)

! Libraries =======================================================================================

! User defined modules ============================================================================

implicit none

! Global variables ================================================================================

! - types -----------------------------------------------------------------------------------------
type(Input_Data_tp), intent(In) :: ModelInfo ! Holds info. (name, dir, output dir) of the model
type(Geometry_tp), intent(in)   :: Geometry    ! Holds information about the geometry of the domain
type(DiscretizedNetwork_tp), intent(In) :: Discretization ! Holds the discretized network
type(partitioner_tp(edges=*, nodes=*)), intent(in)    :: NetworkPartitioner
class(NetworkGeometry_tp), intent(inout) :: this

! Local variables =================================================================================
! - integer variables -----------------------------------------------------------------------------
integer(kind=Tiny) :: nPartition    ! indicates whether a reach is divided btw two ranks (=2) or
                                    ! not (=1)

integer(kind=Tiny) :: i_Partition   ! loop index on the division of a reach for a rank

integer(kind=Smll) :: ERR_Alloc, ERR_DeAlloc ! Allocating and DeAllocating errors

integer(kind=Lng)  :: i_rank        ! loop index on the number of partitions
integer(kind=Lng)  :: i_reach       ! loop index on the reach number
integer(kind=Lng)  :: i_cell        ! loop index on the cell number
integer(kind=Lng)  :: RankNodeI     ! Temp var to hold the rank no. of the firs node of each reach
integer(kind=Lng)  :: RankNodeII    ! Temp var to hold the rank no. of the firs node of each reach
integer(kind=Lng)  :: RankNo        ! Rank number
integer(kind=Lng)  :: NoCellsReach  ! no. of cells from this reach on this rank
integer(kind=Lng)  :: RangeCell_I   ! Temp var to hold the cell no. a reach on the current rank
integer(kind=Lng)  :: RangeCell_II  ! Temp var to hold the cell no. a reach on the current rank

integer(kind=Shrt), dimension(:,:), allocatable :: dset_data_int  ! Data buffers

integer(kind=Shrt) :: rank = 2 ! Dataset rank
integer :: error    ! Error flag

! - real variables --------------------------------------------------------------------------------
real(kind=Dbl), dimension(:,:), allocatable :: dset_data_real ! Data buffers

! - character variables ---------------------------------------------------------------------------
character(kind = 1, len=3   ), parameter :: geo = "Geo"
Character(kind = 1, len = 100 ):: IndexReach !Reach no in the Char. fmt to add to input file Name
Character(kind = 1, len = 100 ):: IndexRank  !Rank no in the Char. fmt to add to input file Name


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

! creating the HDF5 files
call h5open_f(error)

  do i_reach = 1, Geometry%Base_Geometry%NoReaches

    ! The rank which the first node of this reach belongs to
    RankNodeI  = NetworkPartitioner%ReachPartition(i_reach,1)

    ! The rank which the second node of this reach belongs to
    RankNodeII = NetworkPartitioner%ReachPartition(i_reach,2)

    if (RankNodeI == RankNodeII ) then
      nPartition = 1_Tiny
    else if (RankNodeI /= RankNodeII ) then
      nPartition = 2_Tiny
    end if

    ! Whether the reach is divided between the two ranks or not, we need to write the first part.
    ! If it happens that the reach is divided between two ranks, then we need to repeat the loop
    ! one more time
    do i_Partition = 1, nPartition

        if (i_Partition == 1_Tiny ) then
          ! the entire reach belongs to one reach or working on the upstream of the reach

          RankNo = RankNodeI ! the rank no. that this part of the reach belongs to
          NoCellsReach = NetworkPartitioner%ReachPartition(i_reach, 3) ! no. of cells on this rank

          ! defining the range of cell no from this reach on this rank
          RangeCell_I  = 1_Lng
          RangeCell_II = NetworkPartitioner%ReachPartition(i_reach,3)

        else if (i_Partition == 2_Tiny ) then
          ! the reach is divided btw two ranks, here we write the downstream section of the reach

          RankNo = RankNodeII ! the rank no. that this part of the reach belongs to
          NoCellsReach = NetworkPartitioner%ReachPartition(i_reach, 4) ! no. of cells on this rank

          ! defining the range of cell no from this reach on this rank
          RangeCell_I  = NetworkPartitioner%ReachPartition(i_reach,3) + 1_Lng
          RangeCell_II = NetworkPartitioner%ReachPartition(i_reach,3) + &
                                                       NetworkPartitioner%ReachPartition(i_reach,4)

        end if

      ! Converting numbers to char for the output file name (geometry)
      write(IndexRank, *) RankNo    ! converts rank to Character format for the file Name
      write(IndexReach,*) i_reach   ! converts reach no. to Chr format for the file Name

      ! creating the geometry hdf5 file each reach in each rank

      call h5fcreate_f( trim(ModelInfo%InputDir)//'/'//trim(geo)//'_Rank_'// &
                   trim(adjustL(IndexRank))//'_Reach_'//trim(adjustL(IndexReach))//'.h5', &
                   H5F_ACC_TRUNC_F, id_Geometry, error)

      ! working on the coordinate section of the geometry file ---
      dims(1) = 3             ! dimension (3D)
      dims(2) = NoCellsReach  ! no. of cells from this reach on this rank

      ! creating the data space for the coordinates of the cells
      call h5screate_simple_f(rank, dims, dspace_id_XYZ, error)

      ! creating the data set for the coordinates of the cells
      call h5dcreate_f(id_Geometry, "XYZ",  H5T_NATIVE_DOUBLE, dspace_id_XYZ, dset_id_XYZ, error)

      ! allocating a real vector to transmit the coordinate data to the geometry file
      allocate(dset_data_real( dims(1), dims(2)) , stat=ERR_Alloc)
      if (ERR_Alloc /= 0) call error_in_allocation(ERR_Alloc)

      ! transferring the coordinates from the geometry class to paraview class
      dset_data_real(1,:) = this%GeometryReach(i_reach)%XCell(RangeCell_I:RangeCell_II)
      dset_data_real(2,:) = this%GeometryReach(i_reach)%YCell(RangeCell_I:RangeCell_II)
      dset_data_real(3,:) = this%GeometryReach(i_reach)%ZCell(RangeCell_I:RangeCell_II)

      ! Write the dataset- xyz coordinates
      write (*,fmt='("writing coordinates for reach no.: ", I6, " on the rank: ", I6)') &
                                                                                    i_reach, RankNo
      call h5dwrite_f(dset_id_XYZ, H5T_NATIVE_DOUBLE, dset_data_real, dims, error)

      DEallocate(dset_data_real, stat = ERR_DeAlloc )
      if (ERR_DeAlloc /= 0) call error_in_deallocation(ERR_DeAlloc)

     ! working on the connectivity section of the geometry file ---
      dims(1) = 1             ! dimension
      dims(2) = NoCellsReach  ! no. of cells from this reach on this rank

      ! creating the data space for the connectivity of the cells
      call h5screate_simple_f(rank, dims, dspace_id_CNN, error)

      ! creating the data set for the connectivity of the cells
      call h5dcreate_f(id_Geometry, "Connectivity", H5T_NATIVE_INTEGER, dspace_id_CNN, &
                                                                               dset_id_CNN, error)
      allocate(dset_data_int( dims(1), dims(2)) , stat=ERR_Alloc)
      if (ERR_Alloc /= 0) call error_in_allocation(ERR_Alloc)

      ! computing the reach connectivity
      forall (i_cell = 1:NoCellsReach) dset_data_int(1, i_cell) = i_cell - 1_Lng
      !  do i_cell = 1, NoCellsReach
      !    dset_data_int(1, i_cell ) = i_cell - 1_Lng
      !  end do

      ! Write the dataset- connectivity
      write (*,fmt='( "writing connectivity for reach no.: ", I6, " on the rank: ", I6 )') &
                                                                                    i_reach, RankNo
      call h5dwrite_f(dset_id_CNN, H5T_NATIVE_INTEGER, dset_data_int, dims, error)

      DEallocate(dset_data_int, stat = ERR_DeAlloc )
      if (ERR_DeAlloc /= 0) call error_in_deallocation(ERR_DeAlloc)

      ! closing the hdf5 files
      call h5dclose_f(dset_id_XYZ, error)
      call h5dclose_f(dset_id_CNN, error)
      call h5fclose_f(id_Geometry, error)
    end do

  end do

! closing the hdf5 library
call h5close_f(error)


write(*,        fmt="(' Creating the geometry files for Paraview was successful. ')")
write(FileInfo, fmt="(' Creating the geometry files for Paraview was successful. ')")

write(*,       *) " end subroutine < paraview_HDF5_sub >"
write(FileInfo,*) " end subroutine < paraview_HDF5_sub >"

write(*,       *)
write(FileInfo,*)

return
end subroutine paraview_HDF5_sub

end module paraview_mod


