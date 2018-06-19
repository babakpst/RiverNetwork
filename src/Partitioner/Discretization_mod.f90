
!##################################################################################################
! Purpose: This module discretizes the domain.
!
! Developed by: Babak Poursartip
! Supervised by: Clint Dawson
!
! The Institute for Computational Engineering and Sciences (ICES)
! The University of Texas at Austin
!
! ================================ V E R S I O N ==================================================
! V0.00: 02/26/2018 - Start the module.
! V0.01: 02/26/2018 - Initiation: Compiled without error for the first time.
! V0.10: 03/08/2018 - Initiated: Compiled without error.
! V1.00: 03/20/2018 - Compiled with no error/warnings.
! V1.10: 04/10/2018 - Minor modifications in the objects/classes.
! V2.10: 05/24/2018 - modifying for MPI
! V2.20: 05/30/2018 - Initializing types
! V3.20: 06/19/2018 - network discretization
!
! File version $Id $
!
! Last update: 06/19/2018
!
! ================================ S U B R O U T I N E ============================================
! Discretize_1D:  Discretizes the 1D model.
! Domain_Func_1D: Determines the shape of domain
!
! ================================ F U N C T I O N ================================================
!
! ================================ L O C A L   V A R I A B L E S ==================================
! (Refer to the main code to see the list of imported variables)
!  . . . . . . . . . . . . . . . . variables . . . . . . . . . . . . . . . . . . . . . . . . . . .
!
!##################################################################################################

module Discretization_mod

! Libraries =======================================================================================

! User defined modules ============================================================================
use Parameters_mod
use Input_mod, only: Input_Data_tp
use Model_mod, only: Geometry_tp
use Results_mod, only: Plot_domain_1D_tp
use messages_and_errors_mod

implicit none
private

! contains all information after discretization
type DiscretizedReach_tp
  integer (kind=Lng)  :: NCells_reach=0_Lng ! number of cells in the reach

  real(kind=DBL), allocatable, dimension(:) :: LengthCell     ! the length of each cell
  real(kind=DBL), allocatable, dimension(:) :: CellSlope      ! slope of each cell at the center
  real(kind=DBL), allocatable, dimension(:) :: InterfaceSlope ! slope of each cell at the center

  ! coordinate
  real(kind=DBL), allocatable, dimension(:) :: ZCell     ! bottom elev. at the center of each cell
  real(kind=DBL), allocatable, dimension(:) :: YCell     ! the coordinates of the cell center
  real(kind=DBL), allocatable, dimension(:) :: XCell     ! the coordinates of the cell center

  real(kind=DBL), allocatable, dimension(:) :: ZFull     ! bottom elevation at cells and interfaces
  real(kind=DBL), allocatable, dimension(:) :: YFull     ! coordinates at cells and interfaces
  real(kind=DBL), allocatable, dimension(:) :: XFull     ! coordinates at cells and interfaces

  real(kind=DBL) :: ReachManning          ! the Manning's number of each cell
  real(kind=DBL) :: ReachWidthCell        ! the Manning's number of each cell
  real(kind=DBL) :: CellPorjectionLength  ! the length of each cell in the horizontal dir.
end type DiscretizedReach_tp


! contains the information about the entire network, separated by reaches
type DiscretizedNetwork_tp
  integer (kind=Lng)  :: NCells=0_Lng  ! Total number of cells in the network
  real(kind=DBL), allocatable, dimension(:) :: NodeHeight

  ! To hold the discretization of each reach. The size of this type is equal to the no of reaches.
  type(DiscretizedReach_tp), allocatable, dimension(:) :: DiscretizedReach
  contains
    procedure Discretize => Discretize_1D_sub ! subroutine to discretize the reach
end type DiscretizedNetwork_tp

public:: DiscretizedNetwork_tp

contains

!##################################################################################################
! Purpose: This subroutine discretizes the 1D domain and creates the required arrays.
!
! Developed by: Babak Poursartip
! Supervised by: Clint Dawson
!
! The Institute for Computational Engineering and Sciences (ICES)
! The University of Texas at Austin
!
! ================================ V E R S I O N ==================================================
! V0.00: 02/26/2018 - File initiated.
! V0.01: 00/00/2018 - Initiated: Compiled without error for the first time.
! V1.00: 03/20/2018 - Compiled with no error/warnings.
! V1.10: 04/10/2018 - Minor modifications in the objects/classes.
! V2.00: 06/19/2018 - Network discretization
!
! File version $Id $
!
! Last update: 06/19/2018
!
! ================================ L O C A L   V A R I A B L E S ==================================
! (Refer to the main code to see the list of imported variables)
!  . . . . . . . . . . . . . . . . variables . . . . . . . . . . . . . . . . . . . . . . . . . . .
!
!##################################################################################################

subroutine Discretize_network_sub(this, Geometry, ModelInfo)

! Libraries =======================================================================================

! User defined modules ============================================================================

implicit none

! Global variables ================================================================================

! - types -----------------------------------------------------------------------------------------
type(Geometry_tp),   intent(in)   :: Geometry   ! Holds the geometry of the domain.
type(Input_Data_tp), intent(in)   :: ModelInfo  ! Holds info. (name, dir, output dir) of the model

class(DiscretizedNetwork_tp), intent(out) :: this ! Discretization

! Local variables =================================================================================
! - integer variables -----------------------------------------------------------------------------
integer(kind=Smll) :: ERR_Alloc, ERR_DeAlloc ! Allocating and DeAllocating errors

integer(kind=Lng) :: i_Node      ! loop index on the node number in the network
integer(kind=Lng) :: i_reach     ! Loop index on the number of reaches
integer(kind=Lng) :: i_Cell          ! Loop index on the cell numbers for each reach
integer(kind=Lng) :: CellCounter ! Counts number of cells
integer(kind=Lng) :: NetworkOutletNode      ! loop index on the node number in the network
integer(kind=Lng) :: sum_upstream_nodes
integer(kind=Lng) :: Max_Nodes
integer(kind=Lng) :: UpperNode   ! A temporary var that holds the upstream node of a reach.
                                 ! We use this var. to find the height of the network

integer(kind=Tiny), allocatable, dimension(:,:) :: UpstreamNodes ! Indicates all nodes above
                                                                 ! another node

! - real variables --------------------------------------------------------------------------------
real(kind=Dbl)    :: MaxHeight         ! Maximum height of the domain
real(kind=Dbl)    :: Height            ! Height
real(kind=Dbl)    :: Z_loss            ! temp var. to hold loss of height in each cell of reach
real(kind=Dbl)    :: TotalLength       ! Temp var
real(kind=Dbl)    :: CntrlVolumeLength ! The length of control volume
real(kind=Dbl)    :: XCoordinate       ! Temp var to compute the coordinate of the cell center
real(kind=Dbl)    :: ProjectionLength  ! The horizontal length of the reach
real(kind=Dbl)    :: RaisedHeight      ! A temporary variable that holds the difference in height
                                       ! between the upstream and downstream nodes in a reach

logical           :: check_iteration   ! a check parameter on the while loop to make sure that
                                       ! all the upstream nodes are copied
! - type ------------------------------------------------------------------------------------------
type(Plot_domain_1D_tp(NCells=:)), allocatable :: Plot ! Plots the discretized domain

! code ============================================================================================
write(*,       *) " subroutine < Discretize_1D_sub >: "
write(FileInfo,*) " subroutine < Discretize_1D_sub >: "

write(*,       *) " -Discretizing the domain ..."
write(FileInfo,*) " -Discretizing the domain ..."

! Find total number of cells in the domain:
this%NCells = 0_Lng

write(*,        fmt="(A)") " Calculating the total number of the cells in the domain ... "
write(FileInfo, fmt="(A)") " Calculating the total number of the cells in the domain ... "

  do i_reach = 1_Lng,Geometry%NoReaches
    this%NCells = this%NCells + Geometry%NCells_Reach(i_reach)
  end do

write(*,        fmt="(A,I15)") " Total number of cells: ", this%NCells
write(FileInfo, fmt="(A,I15)") " Total number of cells: ", this%NCells

! allocating all items for each reach.
  do i_reach= 1, Geometry%Base_Geometry%NoReaches
    ! allocating each reach in the network
    allocate(
    this%DiscretizedReach(i_reach)%CellSlope(this%DiscretizedReach(i_reach)%NCells_reach),        &
    this%DiscretizedReach(i_reach)%InterfaceSlope(this%DiscretizedReach(i_reach)%NCells_reach+1), &
    this%DiscretizedReach(i_reach)%ZCell(this%DiscretizedReach(i_reach)%NCells_reach),            &
    this%DiscretizedReach(i_reach)%YCell(this%DiscretizedReach(i_reach)%NCells_reach),            &
    this%DiscretizedReach(i_reach)%XCell(this%DiscretizedReach(i_reach)%NCells_reach),            &
    this%DiscretizedReach(i_reach)%ZFull(this%DiscretizedReach(i_reach)%NCells_reach*2_Lng+1_Lng),&
    this%DiscretizedReach(i_reach)%YFull(this%DiscretizedReach(i_reach)%NCells_reach*2_Lng+1_Lng),&
    this%DiscretizedReach(i_reach)%XFull(this%DiscretizedReach(i_reach)%NCells_reach*2_Lng+1_Lng),&
    this%DiscretizedReach(i_reach)%LengthCell(this%DiscretizedReach(i_reach)%NCells_reach ,2),    &
    stat=ERR_Alloc)
    if (ERR_Alloc /= 0) call error_in_allocation(ERR_Alloc)
  end do

! Finding the highest point in the domain:
write(*,        fmt="(A)")" Calculating the height of each node ... "
write(FileInfo, fmt="(A)")" Calculating the height of each node ... "

! Calculation the height of each node =============================================================
  ! Finding out the output node number of the network
  do i_Node = 1_lng, Geometry%Base_Geometry%NoNodes
    if (Geometry%BoundaryCondition(i_Node) == 0_Tiny) then
      NetworkOutletNode = i_Node
      exit
    end if
  end do

! setting the height of the output node - we assume that the height of drain node is zero
this%NodeHeight(NetworkOutletNode) = 0.0_Dbl

  allocate(UpstreamNodes(Geometry%Base_Geometry%NoNodes,Geometry%Base_Geometry%NoNodes),
           stat=ERR_Alloc)
    if (ERR_Alloc /= 0) call error_in_allocation(ERR_Alloc)


! UpstreamNode shows the nodes located at the upstream of each node. Clearly, all nodes are at the
! upstream of the drainage node.
! figuring out what nodes are located at the upstream each node: I do it using a matrix called
! Upstream. if node j is at the upstream of node i the value of upstream(i,j) = 1. This is an
! iterative process. In the first step, we find out the immediate upstream node through the node
! connective of each reach. Next, we add each level of nodes in the upstream in an iterative
! method.

! initializing the nodes
UpstreamNodes(:,:) = 0

! one level up: Initially, we go through all reaches and set the upstream node of each reach at the
! upstream of the drain node.
  do i_reach = 1_Lng, Geometry%Base_Geometry%NoReaches
    UpstreamNodes(Geometry%network(i_reach)%ReachNodes(2),Geometry%network(i_reach)
                                                                             %ReachNodes(1))=1_Tiny
  end do

! In the next step, through an iterative method, we go through all nodes and we do the following
! process for each node. We do the following process repeatedly, until we make sure that we
! recognized all upstream nodes.
! The idea for recognizing the upstream node is that for each node, we read the upstream nodes,
! one by one, and we add the corresponding upstream nodes here. (see the figure.)
! When to stop this process: If we include all upstream nodes in this matrix, then the sum of all
! entries of this matrix would be constant. Thus, we keep track of the sum of all entries, once it
! becomes constant, we stop the process.

check_iteration == .true.
Max_Nodes = 0
  do while (check_iteration == .true.)
    sum_upstream_nodes = 0
    check_iteration == .false.
      do i_Node = 1_Lng, Geometry%Base_Geometry%NoNodes
        do i_Node2 = 1_Lng, Geometry%Base_Geometry%NoNodes
          if (UpstreamNodes(i_Node, i_Node2)==1_Tiny) then
            UpstreamNodes(i_Node, :)= UpstreamNodes(i_Node, :) + UpstreamNodes(i_Node2, :)
          end if
        end do
      end do

    ! returning the values equal to one again
      do i_Node = 1_Lng, Geometry%Base_Geometry%NoNodes
        do i_Node2 = 1_Lng, Geometry%Base_Geometry%NoNodes
          if (UpstreamNodes(i_Node, i_Node2)/=1_Tiny .and.
              UpstreamNodes(i_Node, i_Node2)/=0_Tiny) then
            UpstreamNodes(i_Node, i_Node2)=1_Tiny
          end if
        end do
      end do

      do i_Node = 1_Lng, Geometry%Base_Geometry%NoNodes
        do i_Node2 = 1_Lng, Geometry%Base_Geometry%NoNodes
          sum_upstream_nodes = sum_upstream_nodes + UpstreamNodes(i_Node, i_Node2)
        end do
      end do

      if (Max_Nodes < sum_upstream_nodes ) then
        Max_Nodes = sum_upstream_nodes
        check_iteration == .true.
      end if
  end do


! Now, that we have all the upstream nodes, we figure out the height of each node.
! The idea is that for each reach, we raise the height of the nodes on the upstream of that reach
  do i_reach = 1_Lng, Geometry%Base_Geometry%NoReaches

    ! The difference between the height of upstream and downstream nodes:
    ! (Multiplying the length of each reach by its slope)
    RaisedHeight =  Geometry%network%ReachLength * Geometry%network%ReachSlope

    ! raising the upstream nodes:
    UpperNode = Geometry%network(i_reach)%ReachNodes(1) ! the node at the upstream of the reach

    ! raising the height of the upper node first
    this%NodeHeight(UpperNode) = this%NodeHeight(UpperNode) + RaisedHeight

    ! raising the rest of the nodes on the upstream (we loop over all nodes to see if it is
    ! located at the upstream. If yes, then we raise the height.)
    do i_Node = 1_Lng, Geometry%Base_Geometry%NoNodes
      if (UpstreamNodes(UpperNode, i_Node) = 1_Tiny ) then
        this%NodeHeight(i_Node) = this%NodeHeight(i_Node) + RaisedHeight
      end if
    end do
  end do

write(*,        fmt="(A)") " The height of each node calculated."
write(FileInfo, fmt="(A)") " The height of each node calculated."

! In the following loop we discretize each reach. Note that we discretize each reach from the
! upstream node to the downstream node. That means the cell number attached to the upstream
! node is 1.
write(*,        fmt="(A)")" Loop over reaches to discretize the domain ..."
write(FileInfo, fmt="(A)")" Loop over reaches to discretize the domain ..."


  do i_reach=1_Lng, Geometry%Base_Geometry%NoReaches  ! Loop over the reaches

    write(*,        fmt="(A, I10,A)")" -Discretizing reach no.: ", i_reach, " ..."
    write(FileInfo, fmt="(A, I10,A)")" -Discretizing reach no.: ", i_reach, " ..."

    this%DiscretizedReach(i_reach)%NCells_reach = Geometry%network(i_reach)%NCells_Reach

    CntrlVolumeLength = Geometry%network(i_reach)%ReachLength/
                        Geometry%network(i_reach)%NCells_Reach ! Control volume length

    write(*,fmt="(A,I5,A,F23.10)")" Cell length in the reach ", i_reach," is:", CntrlVolumeLength

    Z_loss = CntrlVolumeLength * Geometry%ReachSlope(i_reach)  ! Height loss in each cell

    ! The height of the upstream node
    Height = this%NodeHeight( Geometry%network(i_reach)%ReachNodes(2))
    ! we raise the initial height by half of Z_loss, which is equivalent to the height of
    ! an imaginary cell center just before the upstream node. To find the height of each cell
    ! thereafter, we reduce Z_loss from Height each time.
    Height = Height + 0.5_Dbl * Z_loss

    XCoordinate = 0.5_Dbl * CntrlVolumeLength ! This would be the coordinate of first cell.

      ! loop on the number of cells of each reach
      do i_Cell = 1_Lng, Geometry%network(i_reach)%NCells_Reach
        this%DiscretizedReach(i_reach)%LengthCell(i_Cell)     = CntrlVolumeLength
        this%DiscretizedReach(i_reach)%CellSlope(i_Cell)      = Geometry%ReachSlope(i_reach)
        this%DiscretizedReach(i_reach)%InterfaceSlope(i_Cell) = Geometry%ReachSlope(i_reach)

        this%DiscretizedReach(i_reach)%XCell(i_Cell)             = XCoordinate
        this%DiscretizedReach(i_reach)%XFull(i_Cell*2_Lng-1_Lng) =
                                                              XCoordinate-0.5_Dbl*CntrlVolumeLength
        this%DiscretizedReach(i_reach)%XFull(i_Cell*2_Lng      ) = XCoordinate

        XCoordinate = XCoordinate + CntrlVolumeLength
        Height      = Height - Z_loss

        this%DiscretizedReach(i_reach)%ZCell(i_Cell)             = Height
        this%DiscretizedReach(i_reach)%ZFull(i_Cell*2_Lng-1_Lng) = Height + 0.5_Dbl * Z_loss
        this%DiscretizedReach(i_reach)%ZFull(i_Cell*2_Lng      ) = Height

        this%DiscretizedReach(i_reach)%ReachManning = Geometry%network(i_reach)%ReachManning
        this%DiscretizedReach(i_reach)%ReachWidthCell = Geometry%network(i_reach)%ReachWidth
      end do

!!!! up to here
          this%DiscretizedReach(i_reach)%XFull(i_Cell*2_Lng      ) = XCoordinate + 0.5000====


    i_Cell = i_Cell + 1_Lng
    this%DiscretizedReach(i_reach)%LengthCell(i_Cell,1) = Geometry%ReachLength(i_reach) - TotalLength

    this%DiscretizedReach(i_reach)%CellSlope(i_Cell)  = Geometry%ReachSlope(i_reach)
    this%DiscretizedReach(i_reach)%InterfaceSlope(i_Cell) = Geometry%ReachSlope(i_reach)

    XCoordinate = XCoordinate-0.5_dbl*CntrlVolumeLength+0.5_dbl*this%DiscretizedReach(i_reach)%LengthCell(i_Cell,1)

    this%DiscretizedReach(i_reach)%XCell(i_Cell)     = XCoordinate
    this%DiscretizedReach(i_reach)%XFull(i_Cell*2_Lng)       = XCoordinate - 0.5 * this%DiscretizedReach(i_reach)%LengthCell(i_Cell,1)
    this%DiscretizedReach(i_reach)%XFull(i_Cell*2_Lng+1_Lng) = XCoordinate
    this%DiscretizedReach(i_reach)%XFull(i_Cell*2_Lng+2_Lng) = XCoordinate + 0.5 * this%DiscretizedReach(i_reach)%LengthCell(i_Cell,1)

    Height = Height - (0.5_dbl * Z_loss + 0.5_dbl * this%DiscretizedReach(i_reach)%LengthCell(i_Cell,1) &
                                                  * Geometry%ReachSlope(i_reach))

    this%DiscretizedReach(i_reach)%ZCell(i_Cell)             = Height

    this%DiscretizedReach(i_reach)%ZFull(i_Cell*2_Lng)       = Height + 0.5_dbl * this%DiscretizedReach(i_reach)%LengthCell(i_Cell,1) &
                                                           * Geometry%ReachSlope(i_reach)
    this%DiscretizedReach(i_reach)%ZFull(i_Cell*2_Lng+1_Lng) = Height
    this%DiscretizedReach(i_reach)%ZFull(i_Cell*2_Lng)       = Height - 0.5_dbl * this%DiscretizedReach(i_reach)%LengthCell(i_Cell,1) &
                                                           * Geometry%ReachSlope(i_reach)
    this%DiscretizedReach(i_reach)%ManningCell(i_Cell)= Geometry%ReachManning(i_reach)
    this%DiscretizedReach(i_reach)%WidthCell(i_Cell)  = Geometry%ReachWidth(i_reach)

    MaxHeight = MaxHeight - Geometry%ReachLength(i_reach) * Geometry%ReachSlope(i_reach)

  end do


write(*,        fmt="(' Discretization was successful. ')")
write(FileInfo, fmt="(' Discretization was successful. ')")

write(*,        fmt="(' -Plotting the discretized domain ... ')")
write(FileInfo, fmt="(' -Plotting the discretized domain ... ')")

! Plot the discretized domain (cell centers)
allocate(Plot_domain_1D_tp(i_Cell) :: Plot, stat=ERR_Alloc)
if (ERR_Alloc /= 0) call error_in_allocation(ERR_Alloc)

! Filling the coordinates for plot
Plot%XCoor(:)      = this%XCell(:)
Plot%ZCoor(:)      = this%ZCell(:)
Plot%CellSlope(:)  = this%CellSlope(:)
Plot%IndexSize     = Geometry%IndexSize

call Plot%plot(ModelInfo)

write(*,       *) " end subroutine < Discretize_1D_sub >"
write(FileInfo,*) " end subroutine < Discretize_1D_sub >"

return
end subroutine Discretize_network_sub

!##################################################################################################
! Purpose: This function determines the shape of the 1D domain.
!
! Developed by: Babak Poursartip
! Supervised by: Clint Dawson
!
! The Institute for Computational Engineering and Sciences (ICES)
! The University of Texas at Austin
!
! ================================ V E R S I O N ==================================================
! V0.00: 02/26/2018 - File initiated.
! V0.01: 02/26/2018 - Initiated: Compiled without error for the first time.
!
! File version $Id $
!
! Last update: 02/26/2018
!
! ================================ L O C A L   V A R I A B L E S ==================================
! (Refer to the main code to see the list of imported variables)
!  . . . . . . . . . . . . . . . . variables . . . . . . . . . . . . . . . . . . . . . . . . . . .
! x: The location of the domain
! Bathymetry: The height of the domain
! DBathymetry: The derivative of the domain at one specific point.
!
!##################################################################################################

function Domain_Func_1D(x) result(Bathymetry)

implicit none

real(kind=Dbl)  :: x
real(kind=Dbl)  :: Bathymetry


! code ============================================================================================

Bathymetry = 0.2_Dbl - 0.05_Dbl * (x-10.0_Dbl)**2

end function Domain_Func_1D

!##################################################################################################
function Domain_Func_1D_D(x) result(DBathymetry)

implicit none

real(kind=Dbl) :: x
real(kind=Dbl) :: DBathymetry

! code ============================================================================================
DBathymetry = - 0.05_Dbl * 2.0_Dbl * (x-10.0_Dbl)

end function Domain_Func_1D_D


!#####################################
function Domain_Func_1D_MacDonald(x) result(Bathymetry)

implicit none

real(kind=Dbl)  :: x
real(kind=Dbl)  :: Bathymetry

real(kind=Dbl)  :: a1 = 0.674202
real(kind=Dbl)  :: a2 = 21.7112
real(kind=Dbl)  :: a3 = 14.492
real(kind=Dbl)  :: a4 = 1.4305

! code ============================================================================================

  if (0.0<= x .and. x <= 200.0_dbl/3.0_dbl) then
    Bathymetry = ((4.0_dbl/Gravity)**(1.0_dbl/3.0_dbl)) * ( 4.0_dbl/3.0_dbl - x/100.0_dbl ) &
               - (9*x/1000.0_dbl) * ( x/100.0_dbl - 2.0_dbl/3.0_dbl)
  else if (200.0_dbl/3.0_dbl <= x .and. x <= 100.0_dbl) then
    Bathymetry = ((4.0_dbl/Gravity)**(1.0_dbl/3.0_dbl))*( &
                 +a1 * (x/100.0_dbl - 2.0_dbl/3.0_dbl)**4 &
                 +a1 * (x/100.0_dbl - 2.0_dbl/3.0_dbl)**3 &
                 -a2 * (x/100.0_dbl - 2.0_dbl/3.0_dbl)**2 &
                 +a3 * (x/100.0_dbl - 2.0_dbl/3.0_dbl) &
                 + a4 &
                 )
  else
    write(*,*)" The defined bathymetry in the discretization_mod is wrong.",x
    stop
  end if

end function Domain_Func_1D_MacDonald

!##################################################################################################
function Domain_Func_1D_MacDonald_D(x) result(DBathymetry)

implicit none

real(kind=Dbl) :: x
real(kind=Dbl) :: DBathymetry

real(kind=Dbl)  :: a1 = 0.674202
real(kind=Dbl)  :: a2 = 21.7112
real(kind=Dbl)  :: a3 = 14.492
real(kind=Dbl)  :: a4 = 1.4305

! code ============================================================================================

  if (0.0<= x .and. x <= 200.0_dbl/3.0_dbl) then
    DBathymetry = ((4.0_dbl/Gravity)**(1.0_dbl/3.0_dbl)) * ( - 1.0_dbl/100.0_dbl ) &
               - (9/1000.0_dbl) * ( x/100.0_dbl - 2.0_dbl/3.0_dbl) &
               - (9*x/1000.0_dbl) * ( 1.0_dbl/100.0_dbl)
  else if (200.0_dbl/3.0_dbl <= x .and. x <= 100.0_dbl) then
    DBathymetry = ((4.0_dbl/Gravity)**(1.0_dbl/3.0_dbl))*( &
                 +4.0_dbl * a1 * (1.0_dbl/100.0_dbl) * (x/100.0_dbl - 2.0_dbl/3.0_dbl)**3 &
                 +3.0_dbl * a1 * (1.0_dbl/100.0_dbl) * (x/100.0_dbl - 2.0_dbl/3.0_dbl)**2 &
                 -2.0_dbl * a2 * (1.0_dbl/100.0_dbl) * (x/100.0_dbl                  )**1 &
                 +a3 * (1.0_dbl/100.0_dbl) &
                 )
  else
    write(*,*)" The defined bathymetry in the discretization_mod is wrong.",x
    stop
  end if

end function Domain_Func_1D_MacDonald_D

end module Discretization_mod
