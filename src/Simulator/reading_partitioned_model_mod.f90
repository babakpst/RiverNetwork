
!##################################################################################################
! Purpose: This module reads all data for simulation.
!
! Developed by: Babak Poursartip
! Supervised by: Clint Dawson
!
! The Institute for Computational Engineering and Sciences (ICES)
! The University of Texas at Austin
!
! ================================ V E R S I O N ==================================================
! V0.10: 02/22/2018 - Initiation.
! V0.10: 03/08/2018 - Initiated: Compiled without error.
! V1.00: 04/20/2018 - Major modifications
! V2.00: 05/15/2018 - Separating model from the input
! V3.00: 07/31/2018 - Modifying the input subroutine to read a partitioned network
!
! File version $Id $
!
! Last update: 07/31/2018
!
! ================================ S U B R O U T I N E ============================================
! reading_partitioned_network
!
! ================================ F U N C T I O N ================================================
!
! ================================ L O C A L   V A R I A B L E S ==================================
! (Refer to the main code to see the list of imported variables)
!  . . . . . . . . . . . . . . . . Variables . . . . . . . . . . . . . . . . . . . . . . . . . . .
!
!##################################################################################################

module reading_network_mod

use Parameters_mod
use Input_mod
use messages_and_errors_mod

implicit none
private

! contains all information about individual reaches after discretization and partitioning
type DiscretizedReach_tp
  integer(kind=Lng)  :: NCells_reach = 0_Lng ! number of cells in the reach
  integer(kind=Lng)  :: ReachNumber  = 0_Lng ! The reach no. in the unpartitioned network

  integer(kind=Tiny) :: Communication=-1_Tiny! if this reach communicates with other ranks
                                             !-1: the entire rank is on one rank-no communication
                                             ! 1: indicates that we need to communicate with the
                                             !    rank that holds the lower part of the reach
                                             !    (the downstream).
                                             ! 2: indicates that we need to communicate with the
                                             !    rank that holds the upper part of the reach
                                             !    (the upstream).

  integer(kind=Shrt) :: CommRank =-1_Shrt    ! indicates the rank number that a reach needs to
                                             ! communicate with, i.e., if the reach is divided
                                             ! between two ranks.

  integer(kind=Shrt) :: UpstreamReaches (2,2) ! This array holds the upstream reach number attached
                                              ! the upstream node of this reach. The first col
                                              ! holds the global reach number in the unpartitioned
                                              ! network, and the second col holds the local reach
                                              ! number on the rank


  integer(kind=Shrt) :: DownstreamReaches (1,2)! This array holds the downstream reach number attached
                                              ! the downstream node of this reach. The first col
                                              ! holds the global reach number in the unpartitioned
                                              ! network, and the second col holds the local reach
                                              ! number on the rank

  integer(kind=tiny) :: NodeI  = -1_Shrt      ! local node numbering of the upstream node attached to this reach
  integer(kind=tiny) :: NodeII = -1_Shrt      ! local node numbering of the downstream node attached to this reach

  integer(kind=tiny) :: BCNodeI  = -1_Shrt   ! the BC of the upstream node of the reach,
  integer(kind=tiny) :: BCNodeII = -1_Shrt   ! the BC of the downstream node of the reach,
                                             ! -1 not on this rank, 0 connected to other nodes,
                                             !  1 inlet BC, 2 outlet BC

  real(kind=DBL) :: ReachManning         = 0.0_dbl ! the Manning's number of each cell
  real(kind=DBL) :: ReachWidthCell       = 0.0_dbl ! the Width a the reach, the assumption is that the width of the reach is constant
  real(kind=DBL) :: CellPorjectionLength = 0.0_dbl ! the length of each cell in the horizontal dir.

  real(kind=DBL) :: Q_Up = 0.0_dbl         ! input discharge at the inlet node,
  real(kind=DBL) :: CntrlV = 0.0_dbl       ! initial control volume of the reach
  real(kind=DBL) :: CntrlV_ratio = 0.0_dbl ! ratio of change of cntl vol in the length of the reach

  real(kind=DBL), allocatable, dimension(:) :: LengthCell     ! the length of each cell
  real(kind=DBL), allocatable, dimension(:) :: CellSlope      ! slope of each cell at the center
  real(kind=DBL), allocatable, dimension(:) :: InterfaceSlope ! slope of each cell at the center

  ! coordinate
  real(kind=DBL), allocatable, dimension(:) :: ZCell     ! bottom elev. at the center of each cell
  real(kind=DBL), allocatable, dimension(:) :: XCell     ! the coordinates of the cell center

  !real(kind=DBL), allocatable, dimension(:) :: ZFull    ! bottom elevation at cells and interfaces
  !real(kind=DBL), allocatable, dimension(:) :: XFull    ! coordinates at cells and interfaces
end type DiscretizedReach_tp


! contains the information about the entire network, separated by reaches
type network_tp

  ! Total number of cells from the network on this rank
  integer(kind=Lng) :: TotalNumOfCellsOnThisRank = 0_Lng

  ! Total number of reaches from the network on this rank
  integer(kind=Lng) :: TotalNumOfReachesOnThisRank = 0_Lng

  ! Total number of nodes from the network on this rank
  integer(kind=Lng) :: TotalNumOfNodesOnThisRank = 0_Lng

  ! Holds number of reaches on this rank that have been cut, we need this info for MPI send/receive
  integer(kind=Lng) :: NCutsOnRanks

  ! Total number of reaches  in the network
  integer(kind=Lng) :: TotalNumOfReachesInTheNetwork = 0_Lng

  ! no. of reaches on each rank, needed for paraview
  integer(kind=Lng), allocatable, dimension(:) :: NoReachonRanks

  ! To hold the discretization of each reach. The size of this type is equal to the no of reaches.
  type(DiscretizedReach_tp), allocatable, dimension(:) :: DiscretizedReach

  contains
    procedure network => reading_partitioned_network
end type network_tp

public:: network_tp

interface

!##################################################################################################
! Purpose: This subroutine reads the partitioned data created by the partitioner. All the types and
! the variables are based on the partitioner code.
!
! Developed by: Babak Poursartip
! Supervised by: Clint Dawson
!
! The Institute for Computational Engineering and Sciences (ICES)
! The University of Texas at Austin
!
! ================================ V E R S I O N ==================================================
! V0.1: 02/23/2018 - Initiation.
! V1.0: 03/01/2018 - Compiled with no errors/warnings.
! V1.0: 04/10/2018 - Minor modifications in the class.
! V2.0: 04/20/2018 - Parallel.
! V2.1: 05/24/2018 - Parallel with MPI
! V3.0: 07/31/2018 - reading the partitioned network
!
! File version $Id $
!
! Last update: 07/31/2018
!
! ================================ L O C A L   V A R I A B L E S ==================================
! (Refer to the main code to see the list of imported variables)
!  . . . . . . . . . . . . . . . . Variables . . . . . . . . . . . . . . . . . . . . . . . . . . .
!
!##################################################################################################

module subroutine reading_partitioned_network(this, ModelInfo)

! Libraries =======================================================================================

! User defined modules ============================================================================

Implicit None

! Global Variables ================================================================================

! - types -----------------------------------------------------------------------------------------
type(Input_Data_tp), intent(In) :: ModelInfo  ! Holds info. (name, dir, output dir) of the model
class(network_tp),  intent(inout) :: this       ! Holds the entire model

end subroutine reading_partitioned_network

end interface

end module reading_network_mod
