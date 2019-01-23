
!##################################################################################################
! Purpose: This module partitions the network and creates the input file for each process/rank.
!          To partition the network, we are using METIS graph partitioner. At the time of
!          developing this code, we have two versions of METIS: V4.0 and V5.X. We develop the code
!          such that both options are available. The main reason for having both options, and not
!          not just having the latest version, is that, based on our experience, the older version
!          handles the partitioning more efficiently.
!          The other option is using ParMetis, which is the parallel version of the METIS. We do
!          not believe that ParMETIS is needed. Metis would be efficient enough.
!
! Developed by: Babak Poursartip
! Supervised by: Clint Dawson
!
! The Institute for Computational Engineering and Sciences (ICES)
! The University of Texas at Austin
!
! ================================ V E R S I O N ==================================================
! V0.00: 04/15/2018 - File initiated.
! V2.00: 04/17/2018 - Debugged successfully.
! V3.00: 06/25/2018 - Modifying the partitioner module to partition a network.
! V3.10: 07/23/2018 - Adding one rank partitioner
! V3.10: 07/26/2018 - modification on the paritioning to find total number of reaches on each rank
!
! File version $Id: $
!
! Last update: 07/26/2018
!
! ================================ S U B R O U T I N E ============================================
! Network_Partitioner_Sub: Creates the input files for various processes.
!
! ================================ F U N C T I O N ================================================
!
! ================================ L O C A L   V A R I A B L E S ==================================
! (Refer to the main code to see the list of imported variables)
!  . . . . . . . . . . . . . . . . Variables . . . . . . . . . . . . . . . . . . . . . . . . . . .
!
!##################################################################################################

module Network_Partitioner_mod

! Libraries =======================================================================================

! User defined modules ============================================================================
use Parameters_mod
use messages_and_errors_mod
use Discretize_the_network_mod, only: DiscretizedNetwork_tp
use Input_mod,                  only: Input_Data_tp
use Model_mod,                  only: Geometry_tp
use Results_mod,                only: Plot_domain_1D_tp
use iso_c_binding


implicit none

! We define this type for so that we can define a linked list based on that. We use this type to
! find the nodes connected to each node.
type NodeConncetivity_tp
 integer :: nodes
 integer :: cells
 type(NodeConncetivity_tp), pointer :: next => null()
end type NodeConncetivity_tp

type NodeConncetivityArray_tp
  integer :: counter = 0
  type(NodeConncetivity_tp), pointer :: head => null()
end type NodeConncetivityArray_tp

! This type contains all the variables required to partition a graph using METIS version 4.0.0
! We define the required variables as pointers, because it is described in the METIS manual. Using
! non-pointer variables is also possible, but, to have more control over the METIS options, we
! prefer pointers.
! The notation here (variables) is identical to the notation in the METIS 4 manual.
! Remark: All the METIS 4 arguments, according the manual, are pointers. In this code, we define
! all variables as pointers, however, it is possible to consider all the arguments as non-pointers.
type METIS_var4_tp

  ! The number of vertices in the graph= NoNodes in the network
  integer(kind=Shrt), pointer :: n

  ! The adjacency structure of the graph as described in Section 5.1. The size of array is n+1
  integer(kind=Shrt), pointer, dimension(:) :: xadj => null()

  ! The adjacency structure of the graph as described in Section 5.1.
  ! The size of this array is "2*(number of edges(=no or reaches))"
  integer(kind=shrt), pointer, dimension(:) :: adjncy => null()

  ! The weights of the vertices as described in Section 5.1.
  ! This array stays null in our case. The size of this array is nvtxs * ncon
  type(c_ptr) :: vwgt

  ! The weights of the edges as described in Section 5.5. The size of this array is 2 * m,
  ! where m is the total number of edges.
  integer(kind=Shrt), pointer, dimension(:) :: adjwgt => null()


  ! Used the indicate if the graph is weighted. (see page 22 of manual of version 4).
  ! wgtflags can take the following values:
  ! 1 weights on the edges only (vwgts = NULL)
  integer(kind=Shrt), pointer :: wgtflag => null()

  ! Indicated the C or Fortran style of numbering: 0: C or 1: fortran
  integer(kind=Shrt), pointer :: numflag => null()

  ! The number of parts to partition the graph
  integer(kind=Shrt), pointer :: nparts => null()

  ! see page 22. Use options[0]=0.
  integer(kind=Shrt), pointer, dimension(:) :: options => null()

  ! The number of edges that are cut by the partition. In our case, this indicates the total number
  ! of communication between the ranks.
  integer(kind=Shrt) :: edgecut =-1_shrt

  ! This is a vector of "size nvtxs" that upon successful completion stores the partition
  ! vector of the graph. The numbering of this vector starts from either 0 or 1, depending on
  ! the value of options[METIS OPTION NUMBERING].
  integer(kind=Shrt), pointer, dimension(:) :: part => null()

end type METIS_var4_tp

! This type contains all the variables required to partition a graph using METIS version 5.1.0.
! We define the required variables as pointers, because it is described in the METIS manual. Using
! non-pointer variables is also possible, but, to have more control over the METIS options, we
! prefer pointers.
type METIS_var5_tp
  ! The number of vertices in the graph= NoNodes in the network
  !integer(kind=c_int) :: nvtxs
  integer(kind=Shrt), pointer :: nvtxs

  ! The number of balancing constraints.
  ! ncon is discussed at the beginning of page 10 of the manual.
  !integer(kind=c_int):: ncon
  integer(kind=Shrt), pointer :: ncon ! ncon cannot be a pointer(null), at least ncon = 1

  ! The adjacency structure of the graph as described in Section 5.5. The size of array is nvtxs+1
  !integer(kind=c_int), allocatable, dimension(:) :: xadj
  integer(kind=Shrt), pointer, dimension(:) :: xadj

  ! The adjacency structure of the graph as described in Section 5.5.
  ! The size of this array is "2*(number of edges(=no or reaches))"
  !integer(kind=c_int), allocatable, dimension(:) :: adjncy
  integer(kind=Shrt), pointer, dimension(:) :: adjncy

  ! The weights of the vertices as described in Section 5.5.
  ! This array stays null in our case. The size of this array is nvtxs * ncon
  !integer(kind=c_int), allocatable, dimension(:) :: vwgt
  type(c_ptr) :: vwgt

  ! The size of the vertices for computing the total communication volume as described in
  ! Section 5.7. The size of this array is nvtxs
  !integer(kind=c_int), allocatable, dimension(:) :: vsize
  type(c_ptr) :: vsize

  ! The weights of the edges as described in Section 5.5. The size of this array is 2 * m,
  ! where m is the total number of edges.
  !integer(kind=c_int), allocatable, dimension(:) :: adjwgt
  integer(kind=Shrt), pointer, dimension(:) :: adjwgt
  !type(c_ptr) :: adjwgt

  ! The number of parts to partition the graph
  !integer(kind=c_int) :: nparts
  integer(kind=Shrt), pointer :: nparts

  ! specifies the desired weight for each partition and constraint.
  ! The size of the array nparts×ncon.
  !integer(kind=c_int), allocatable, dimension(:) :: tpwgts
  type(c_ptr) ::tpwgts

  ! This is an array of size ncon that specifies the allowed load imbalance tolerance
  ! for each constraint.
  !integer(kind=Shrt), pointer, dimension(:) :: ubvec
  type(c_ptr) :: ubvec

  ! see page 21.
  !integer(kind=c_int), allocatable, dimension(:) :: options
  !integer(kind=Shrt), pointer, dimension(:) :: options
  type(c_ptr) :: options

  ! Upon successful completion, this variable stores the edge-cut or the total communication volume
  ! of the partitioning solution. The value returned depends on the partitioning’s objective
  ! function.
  !integer(kind=c_int) :: objval
  integer(kind=Shrt) :: objval=-1_shrt
  !integer(kind=Shrt), pointer :: objval

  ! This is a vector of "size nvtxs" that upon successful completion stores the partition
  ! vector of the graph. The numbering of this vector starts from either 0 or 1, depending on
  ! the value of options[METIS OPTION NUMBERING].
  !integer(kind=c_int), allocatable, dimension(:) :: part
  integer(kind=Shrt), pointer, dimension(:) :: part
end type METIS_var5_tp

type partitioner_tp(edges, nodes)
  integer(kind=Shrt), len :: edges  ! number of edges in the network - reaches
  integer(kind=Shrt), len :: nodes  ! number of nodes in the network - junction

  integer :: ncon    = 1_shrt ! Temp variable for graph partitioning.
  integer :: wgtflag = 1_shrt ! Weights on the edges only (vwgts = NULL)
  integer :: numflag = 1_shrt ! Fortran style numbering is assumed that starts from 1

  !integer, dimension(0:nodes-1_Lng) :: part
  integer, dimension(nodes) :: part

  integer, dimension(1:5)   :: options4 = 0_Shrt

  !integer, dimension(METIS_NOPTIONS) :: options5
  integer, dimension(0:39) :: options5
  !integer, dimension(0:40) :: options5
  !type(c_ptr):: options5

  ! This reach holds information about each reach after partitioning. For each reach, the first and
  ! the second col, holds the rank in which the first and the second node of this reach belongs to,
  ! respectively. The third and fourth col, holds the total number of cells corresponding to each
  ! rank. The 5th and 6th cols indicate the local reach number on the rank. If both node are on the
  ! same rank, then we have a local number in the 5th col only, but if the reach is shared between
  ! For example, ... (to be continued) <modify>
  integer(kind=Lng), dimension(edges,6) :: ReachPartition

  ! We go with the C style numbering- Arrays start from zero.
  !integer, dimension(0:nodes-1)    :: vwgt_target   ! Read METIS manual for details.
  !integer, dimension(0:nodes-1)    :: vsize_target  ! Read METIS manual for details.
  !integer, dimension(0:nodes+1-1)  :: xadj_target   ! Read METIS manual for details.
  !integer, dimension(0:2*edges-1)  :: adjncy_target ! Read METIS manual for details.
  !integer, dimension(0:2*edges-1)  :: adjwgt_target ! Read METIS manual for details.

  !real, allocatable, dimension(:) :: tpwgts
  !type(c_ptr):: tpwgts

  !integer, dimension(nodes)    :: vwgt_target   ! Read METIS manual for details.
  !integer, dimension(nodes)    :: vsize_target  ! Read METIS manual for details.
  integer, dimension(nodes+1)  :: xadj_target   ! Read METIS manual for details.
  integer, dimension(2*edges)  :: adjncy_target ! Read METIS manual for details.
  integer, dimension(2*edges)  :: adjwgt_target ! Read METIS manual for details.

  type(METIS_var4_tp) :: METIS4
  type(METIS_var5_tp) :: METIS5
  contains
    procedure :: Partition => Network_Partitioner_Sub

end type partitioner_tp

contains

!##################################################################################################
! Purpose: This subroutine creates the input file for each partition/process. To this aim, we are
! using METIS graph partitioner. We treat nodes (the upstream and downstream point of each reach)
! as vertices, and reaches as edges. Since the length of each reach is different, consequently, the
! number of cells are different in each reach, to have a balanced load distribution, we use a
! "weighted" graph partitioner subroutine/ function in METIS, as follows:
!
! 1- If we use METIS v4.0, to partition a graph, we have two options (see page 8 of the METIS
! manual v4.0): one method is "pmetis", and the other one is "kmetis". According the instruction in
! the manual, kmetis is more efficient to partition a model to more than 8 partitions. Therefore,
! we only use this option. The corresponding subroutine/function is METIS_PartGraphKway, discussed
! on page 22 of the manual.
!
! 2- If we use METIS V5.1.0, the only option for graph partitioning is METIS_PartGraphKway,
! described on page 26.
!
! The arguments of the two subroutines are different. We design this code the latest version.
!
! Developed by: Babak Poursartip
!
! The Institute for Computational Engineering and Sciences (ICES)
! The University of Texas at Austin
!
! ================================ V E R S I O N ==================================================
! V0.00: 04/15/2018 - Subroutine initiated.
! V0.01: 04/16/2018 - Initiated: Compiled without error for the first time.
! V1.00: 04/20/2018 - Make it compatible with the main code
! V2.00: 07/02/2018 - Modifying the subroutine Discretization to partition a network,
!                     using METIS graph partitioner
!
! File version $Id $
!
! Last update: 06/21/2018
!
! ================================ L O C A L   V A R I A B L E S ==================================
! (Refer to the main code to see the list of imported variables)
!  . . . . . . . . . . . . . . . . Variables . . . . . . . . . . . . . . . . . . . . . . . . . . .
!
!##################################################################################################

subroutine Network_Partitioner_Sub(this, Geometry, Discretization, ModelInfo)

implicit none

! Global variables ================================================================================

! - types -----------------------------------------------------------------------------------------
type(Input_Data_tp), intent(In) :: ModelInfo ! Holds info. (name, dir, output dir) of the model
type(Geometry_tp),   intent(In) :: Geometry  ! Holds the geometry of the network
type(DiscretizedNetwork_tp), intent(In) :: Discretization ! Holds the discretized network

! defining the variables to partition a network using METIS
class(partitioner_tp(edges=*, nodes=*)), intent(inout), target :: this

! Local variables =================================================================================
! - integer variables -----------------------------------------------------------------------------
integer(kind=tiny) :: Communication ! indicates if a reach needs to communicate with other ranks
                                    ! i.e. if the entire reach is on one rank or not.
                                    ! -1: no communication- the entire reach is on one partition
                                    !  1: communicate with the rank that holds the lower part of
                                    !     the reach (downstream).
                                    !  2: communicate with the rank that holds the upper part of
                                    !     the reach (upstream).

integer(kind=tiny) :: BCNodeI       ! the BC of the upstream node of the reach,
integer(kind=tiny) :: BCNodeII      ! the BC of the downstream node of the reach,
                                    ! -1 not on this rank, 0 connected to other nodes,
                                    !  1 inlet BC, 2 outlet BC

integer(kind=Shrt) :: CommRank      ! indicates the rank number that a reach needs to communicate
                                    ! with, i.e., if the reach is divided between two ranks.
                                    ! Important note: Based on the method that we partition the
                                    !                 network, a reach is either on one rank or two

! The assumption is that there are at most three reaches at each junction. Thus, there are two
! upstream reaches (ReachLeft and ReachRight) and one downstream reach (ReachBottom). In order to
! define the local reach numbering for these three reaches at each junction, we define the
! following variables.
integer(kind=Lng)  :: ReachLeft     ! local reach no on the rank for one of upstrm reaches at the junction
integer(kind=Lng)  :: ReachRight    ! local reach no on the rank for the other upstrm reaches at the junction
integer(kind=Lng)  :: ReachBottom   ! local reach no on the rank for dwnstrm reach at the junction

integer(kind=Lng)  :: CellCounter   ! Counter for cells on each partition
integer(kind=Lng)  :: TotalCellCounter ! Counter for the total number of cells
integer(kind=Lng)  :: ReachCounter  ! Counter for reaches
integer(kind=Lng)  :: NodeI, NodeII ! Temp var to hold node number of each reach
integer(kind=Lng)  :: RankNodeI     ! Temp var to hold the rank no. of the firs node of each reach
integer(kind=Lng)  :: RankNodeII    ! Temp var to hold the rank no. of the firs node of each reach
integer(kind=Lng)  :: RangeCell_I   ! Temp var to hold the cell no. a reach on the current rank
integer(kind=Lng)  :: RangeCell_II  ! Temp var to hold the cell no. a reach on the current rank

integer(kind=Smll) :: ERR_Alloc, ERR_DeAlloc ! Allocating and DeAllocating errors

integer(kind=Smll) :: UnFile        ! Holds Unit of a file for error message
integer(kind=Smll) :: IO_File       ! For IOSTAT: Input Output Status in OPEN command
integer(kind=Smll) :: IO_write      ! Used for IOSTAT: Input/Output Status in the write command

integer(kind=Shrt) :: i_rank        ! Loop index for the number of processes/ranks
integer(kind=Shrt) :: i             ! Loop index
integer(kind=Shrt) :: remainder     ! Temp var to see the approximate number of cells in each rank

integer(kind=Lng)  :: i_cells       ! Loop index over cells
integer(kind=Lng)  :: i_reach       ! Loop index over reaches
integer(kind=Lng)  :: i_node        ! Loop index over nodes
integer(kind=Lng)  :: counter       ! counter for the edges

integer(kind=Lng)  :: tempCell      ! Temp var to hold no. of cells of each rank for a shared reach
integer(kind=Lng)  :: Weights       ! Weight of each edge (= no. cells in the edge= reach)
integer(kind=Lng)  :: NodeLocation  ! Temp var to hold the location of adjacent nodes in the graph


! holds the local no of upstream reach  - -1 if upstream of this reach is not on this rank
integer(kind=Lng)  :: UpstreamI
! holds the local no of upstream reach  - -1 if upstream of this reach is not on this rank
integer(kind=Lng)  :: UpstreamII
! holds the local no of downstream reach- -1 if upstream of this reach is not on this rank
integer(kind=Lng)  :: Downstream

! holds the local no of the upstream node- -1 if node is not on this rank
integer(kind=Lng)  :: LocalNodeOfReachI
! holds the local no of the downstream node- -1 if node is not on this rank
integer(kind=Lng)  :: LocalNodeOfReachII

integer, target    :: NumberOfNodes ! saves total number of nodes
integer, target    :: NumberOfRanks ! saves total number of ranks

! counting total number of reaches partitioned- to check the validity of the partitioning
integer(kind=Lng)  :: counter_reach


! - integer Arrays --------------------------------------------------------------------------------
integer(kind=Lng), dimension (Geometry%Base_Geometry%size,4) :: chunk
                         ! share of the network cells for each rank
                         ! col 1: ideal chunk size
                         ! col 2: certain cells, from reaches with both nodes on this rank
                         ! col 3: unsure cells, from ranks with two nodes on two different ranks
                         ! col 4: summation of cols 2 and 3, the actual no. of cells on this ranks

! Indicates how many reaches exist on each rank, we need this for allocating memory in the
! simulator code.
integer(kind=Lng), dimension (Geometry%Base_Geometry%size) :: NReachOnRanks

! Holds total number of nodes on each rank
integer(kind=Lng), dimension (Geometry%Base_Geometry%size) :: NNodesOnRanks

! This array holds all the reach numbers attached to each nodes. The assumption is that at most,
! there are 4 upstream and 4 downstream reaches attached to this node. The first col is used to
! record the number of upstream reaches attached to the node, the second col is used to record the
! number of downstream reaches attached to this node. The next 4 cols keep record of the upstream
! reaches, and the last 4 cols keep record of the downstream reach no.
! This needs to be modified. <modify>
integer(kind=Lng), dimension (Geometry%Base_Geometry%NoNodes, 10) :: ReachAttachedToNode

! This array holds local node numbering on each rank
integer(kind=Lng), dimension (Geometry%Base_Geometry%NoNodes) :: LocalNodeNumbering

! - character variables ---------------------------------------------------------------------------
Character(kind = 1, len = 20) :: IndexRank ! Rank no in the Char. fmt to add to the input file Name
Character(kind = 1, len = 100) :: IndexReach ! Reach no in the Char fmt to add to input file Name

logical :: Balanced_load

! We use this linked list to figure out how many nodes are connected to each node.
type(NodeConncetivityArray_tp), allocatable, dimension(:) :: NodeConnectivity ! Linked list
type(NodeConncetivity_tp), pointer :: Temp ! This is a temporary type to create the linked list
type(Plot_domain_1D_tp(NCells=:)), allocatable :: Plot ! Plots the discretized domain

! code ============================================================================================
write(*,        fmt="(A)") " subroutine < Network_Partitioner_Sub >: "
write(FileInfo, fmt="(A)") " subroutine < Network_Partitioner_Sub >: "

! If the number of partitions are more than one, we use METIS to partition the network, otherwise,
! if there are only one partition, we directly write the information.

if (Geometry%Base_Geometry%size > 1_Shrt) then

! Computing the chunk of each process
write(*,        fmt="(A)") " calculating the average number of cells on each rank ... "
write(FileInfo, fmt="(A)") " calculating the average number of cells on each rank ... "

chunk(:,:) = 0_Lng
remainder  = mod(Discretization%NCells, Geometry%Base_Geometry%size)
chunk(:,1) = (Discretization%NCells - remainder)/Geometry%Base_Geometry%size

  do i = 1_Shrt, remainder
    chunk(i,1) = chunk(i,1) + 1
  end do

write(*,        fmt="(A)") " The ideal cell distribution is: "
write(FileInfo, fmt="(A)") " The ideal cell distribution is: "

  do i_rank = 1_Shrt, Geometry%Base_Geometry%size
    write(*, fmt="( ' rank: ' ,I6, ' no. cells: ', I 12)") i_rank, chunk(i_rank,1)
  end do

! - Prepare for partitioning ----------------------------------------------------------------------
write(*,        fmt="(A)") " -Preparing data for METIS ... "
write(FileInfo, fmt="(A)") " -Preparing data for METIS ... "

! Preparing data for partitioning:

! setting the adjncy and xadj of the graph for METIS ----------------------------------------------
! To fill these vectors, we loop over the reaches, and find the both nodes connected to the reach
! and we add the connectivities to each node. We are using a linked list for this purpose, because
! we do not know in advance, how many nodes are connected to each node.
! At the same time, we fill the adjacency weight (adjwgt), based on the n. of cells in each reach.
allocate(NodeConnectivity(Geometry%Base_Geometry%NoNodes), stat=ERR_Alloc)
if (ERR_Alloc /= 0) call error_in_allocation(ERR_Alloc)

write(*,        fmt="(A)") " Reach connectivities ... "
write(FileInfo, fmt="(A)") " Reach connectivities ... "

  ! This loop figures out all connectivities between nodes
  do i_reach = 1_Lng, Geometry%Base_Geometry%NoReaches
    NodeI   = Geometry%network(i_reach)%ReachNodes(1)
    NodeII  = Geometry%network(i_reach)%ReachNodes(2)
    Weights = Geometry%network(i_reach)%NCells_Reach

    allocate(Temp)
    Temp= NodeConncetivity_tp(NodeII, Weights, NodeConnectivity(NodeI)%head)
    NodeConnectivity(NodeI)%counter = NodeConnectivity(NodeI)%counter + 1_Lng
    NodeConnectivity(NodeI)%head => Temp

    allocate(Temp)
    Temp= NodeConncetivity_tp(NodeI, Weights, NodeConnectivity(NodeII)%head)
    NodeConnectivity(NodeII)%counter = NodeConnectivity(NodeII)%counter + 1_Lng
    NodeConnectivity(NodeII)%head => Temp
  end do

! Now that we have the connectivities, we need to fill xadj, adjncy.
write(*,        fmt="(A)") " Filling the METIS arrays ... "
write(FileInfo, fmt="(A)") " Filling the METIS arrays ... "

! Initialize the the arrays
this%xadj_target(:)   = -1_Shrt
this%adjncy_target(:) = -1_Shrt
this%adjwgt_target(:) = -1_Shrt
this%part(:)          = -1_Shrt

  if (Geometry%Base_Geometry%METIS_version == 1_Tiny) then ! Partitioning using METIS version 5
    !----------------------------------------------------------------------------------------------
    ! C style ----
    !NodeLocation = 0
    !counter      = -1_Lng ! We go with the C style numbering- Arrays start from zero.
    !  do i_node = 1_Lng, Geometry%Base_Geometry%NoNodes
    !    this%xadj_target(i_node-1_Lng) = NodeLocation
    !    NodeLocation = NodeLocation + NodeConnectivity(i_node)%counter
    !    Temp => NodeConnectivity(i_node)%head
    !      do
    !      if (.not.associated(Temp)) exit
    !        counter = counter+1_Lng
    !        this%adjncy_target(counter) = Temp%nodes
    !        this%adjwgt_target(counter) = Temp%cells
    !        Temp => Temp%next
    !      end do
    !  end do

    !this%xadj_target(i_node-1_Lng) = NodeLocation
    !this%adjncy_target(:) = this%adjncy_target(:) - 1_Lng ! C style
    !this%vwgt_target (:) = 1 ! 0
    !this%vsize_target (:) = 0 ! 0

    !  if (counter /= 2_Lng *  Geometry%Base_Geometry%NoReaches-1_Lng) then
    !    write(*,*) " Something is wrong with the adjacency finder"
    !    write(*,*) counter, 2_Lng*Geometry%Base_Geometry%NoReaches-1_Lng
    !    stop
    !  end if

    !----------------------------------------------------------------------------------------------
    ! C style with Fortran arrays ----
    NodeLocation = 0
    counter      = 0_Lng ! We go with the C style numbering- Arrays start from zero.
      do i_node = 1_Lng, Geometry%Base_Geometry%NoNodes
        this%xadj_target(i_node) = NodeLocation
        NodeLocation = NodeLocation + NodeConnectivity(i_node)%counter
        Temp => NodeConnectivity(i_node)%head
          do
            if (.not.associated(Temp)) exit
            counter = counter+1_Lng
            this%adjncy_target(counter) = Temp%nodes
            this%adjwgt_target(counter) = Temp%cells
            Temp => Temp%next
          end do
      end do

    this%xadj_target(i_node) = NodeLocation
    this%adjncy_target(:) = this%adjncy_target(:) - 1_Lng ! C style
    !this%vwgt_target (:) = 0 ! 0
    !this%vsize_target (:) = 0 ! 0

      if (counter /= 2_Lng *  Geometry%Base_Geometry%NoReaches) then
        write(*,*) " Something is wrong with the adjacency finder"
        write(*,*) counter, 2_Lng*Geometry%Base_Geometry%NoReaches-1_Lng
        stop
      end if

  else if (Geometry%Base_Geometry%METIS_version == 0_Tiny) then ! Partitioning using METIS ver. 4
    !----------------------------------------------------------------------------------------------
    ! fortran style (numbering and arrays are all started from 1)
    NodeLocation = 1
    counter      = 0_Lng
      do i_node = 1_Lng, Geometry%Base_Geometry%NoNodes
        this%xadj_target(i_node) = NodeLocation
        NodeLocation = NodeLocation + NodeConnectivity(i_node)%counter
        Temp => NodeConnectivity(i_node)%head
          do
            if (.not.associated(Temp)) exit
            counter = counter+1_Lng
            this%adjncy_target(counter) = Temp%nodes
            this%adjwgt_target(counter) = Temp%cells
            Temp => Temp%next
          end do
      end do

    this%xadj_target(i_node) = NodeLocation
    this%adjncy_target(:) = this%adjncy_target(:) ! Fortran style

      if (counter /= 2_Lng *  Geometry%Base_Geometry%NoReaches) then
        write(*,*) " Something is wrong with the adjacency finder"
        write(*,*) counter, 2_Lng*Geometry%Base_Geometry%NoReaches
        stop
      end if

  end if

NumberOfRanks  = Geometry%Base_Geometry%size
NumberOfNodes  = Geometry%Base_Geometry%NoNodes

! <delete> after debugging
!print*, "xadj",     this%xadj_target ! <delete> after debugging
!print*, "adjncy:",  this%adjncy_target  ! <delete> after debugging
!print*, "adjwgt:",  this%adjwgt_target ! <delete> after debugging
!print*, "vwgt:",    this%vwgt_target
!print*, "partitions", NumberOfRanks

! - partitioning using METIS ----------------------------------------------------------------------
write(*,        fmt="(A)") " -Graph partitioning using METIS_PartGraphKway... "
write(FileInfo, fmt="(A)") " -Graph partitioning using METIS_PartGraphKway ... "

  ! Partitioning using METIS version 5
  if (Geometry%Base_Geometry%METIS_version == 1_Tiny .and. NumberOfRanks /= 1_Shrt) then

    write(*,        fmt="(A)") " Partition the network using METIS 5.1.0 ..."
    write(FileInfo, fmt="(A)") " Partition the network using METIS 5.1.0 ..."

    !call METIS_SetDefaultOptions(this%options5)

    NumberOfNodes = Geometry%Base_Geometry%NoNodes
    this%METIS5%nvtxs => NumberOfNodes ! Setting the number of vertices

    !print*, " options5: ", this%options5
    !call METIS_SetDefaultOptions(this%options5)   ! <modify> using preprocessor derivative
    !call METIS_SetDefaultOptions(this%METIS5%options)   ! <modify> using preprocessor derivative

    !this%options5(METIS_OPTION_OBJTYPE) = METIS_OBJTYPE_CUT
    !this%options5(METIS_OPTION_NCUTS)   = ?
    !this%options5(METIS_OPTION_NUMBERING) = 1
    !this%options5(7) = 1
    !this%options5(17) = 1
    !print*, " options5: ", this%options5

    this%METIS5%nvtxs  => NumberOfNodes ! Setting the number of vertices
    this%ncon          = 1 ! Do not change
                         ! This should be equal to 1, even though in some locations in the manual,
                         ! it is said that we can set it equal to NULL.
    this%METIS5%ncon   =>  this%ncon

    this%METIS5%xadj   => this%xadj_target
    this%METIS5%adjncy => this%adjncy_target

    this%METIS5%vwgt   =  c_null_ptr
    this%METIS5%vsize  =  c_null_ptr

    this%METIS5%adjwgt => this%adjwgt_target

    this%METIS5%nparts => NumberOfRanks

    this%METIS5%tpwgts = c_null_ptr
    this%METIS5%ubvec  = c_null_ptr
    this%METIS5%options= c_null_ptr

    !this%METIS5%options= this%options5
    this%METIS5%part   = this%part

    write(*,        fmt="(A)") " Calling METIS partitioner ... "
    write(FileInfo, fmt="(A)") " Calling METIS partitioner ... "

    ! All attempts to make this subroutine to work, failed.
    !call METIS_PartGraphKway(this%METIS5%nvtxs,   &
    !                         this%METIS5%ncon,    &
    !                         this%METIS5%xadj,    &
    !                         this%METIS5%adjncy,  &
    !                         this%METIS5%vwgt,    &
    !                         this%METIS5%vsize,   &
    !                         this%METIS5%adjwgt,  &
    !                         this%METIS5%nparts,  &
    !                         this%METIS5%tpwgts,  &
    !                         this%METIS5%ubvec,   &
    !                         this%METIS5%options, &
    !                         this%METIS5%objval,  &
    !                         this%METIS5%part)

    write(*,        fmt="(A)") " Done with METIS!!! "
    write(FileInfo, fmt="(A)") " Done with METIS!!!  "
    stop

  ! Partitioning using METIS ver. 4
  else if (Geometry%Base_Geometry%METIS_version == 0_Tiny  .and. NumberOfRanks /= 1_Shrt) then

    write(*,        fmt="(A)") " Partition the network using METIS 4.0.0 ..."
    write(FileInfo, fmt="(A)") " Partition the network using METIS 4.0.0 ..."

    this%METIS4%n       => NumberOfNodes ! Setting the number of vertices
    this%METIS4%xadj    => this%xadj_target
    this%METIS4%adjncy  => this%adjncy_target
    this%METIS4%adjwgt  => this%adjwgt_target
    this%METIS4%wgtflag => this%wgtflag
    this%METIS4%numflag => this%numflag
    this%METIS4%nparts  => NumberOfRanks
    this%METIS4%options => this%options4
    this%METIS4%part    => this%part
    this%METIS4%vwgt    = c_null_ptr

    write(*,        fmt="(A)") " Calling METIS partitioner ... "
    write(FileInfo, fmt="(A)") " Calling METIS partitioner ... "

    call METIS_PartGraphKway(this%METIS4%n,       &
                             this%METIS4%xadj,    &
                             this%METIS4%adjncy,  &
                             this%METIS4%vwgt,    &
                             this%METIS4%adjwgt,  &
                             this%METIS4%wgtflag, &
                             this%METIS4%numflag, &
                             this%METIS4%nparts,  &
                             this%METIS4%options, &
                             this%METIS4%edgecut, &
                             this%METIS4%part)

  else
    write(*,*) " The requested METIS version does not exist. Please, enter 0 for METIS version 4 &
                 or 1 for METIS version 5 in the --.DataModel file."

    call error_in_the_requested_option()
    stop
  end if


! <delete> afte debugging
!  do i = 1, NumberOfNodes
!    write(*,fmt="(2I2)")i, this%part(i)
!  end do

! - analyzing the partitioned graph
! In the following loop, we analyze the partitioning to see how many cells sits on each rank.
! Additionally, we count the number of reaches on each rank.
! We loop over the reaches and we see the ranks the two nodes of the reach are belong to.
! If both nodes of a reach belong to one rank, then we devote the entire cells in this reach, to
! this rank. But, if the two nodes of the reach belong to two different ranks, then, initially, we
! divide the cells evenly between the two ranks. In the next step, we will play with the cells
! until we have an even distribution. We would like to distribute the
! cells as even as possible, so that the share of each rank is as close as possible to the number
! we calculated in the beginning of this subroutine, saved in the "chunk" array.
! At the end of this loop, we have a rough estimation of the partitioning and the no. of cells on
! each rank. In the next step, we try to equalize the no. of cells on each reach.

NReachOnRanks(:) = 0_Lng ! initialize, we set the number of reaches on each rank equal to zero.
NNodesOnRanks(:) = 0_Lng ! initialize, we set the number of Nodes on each rank equal to zero.
LocalNodeNumbering(:) = 0_Lng ! initializing local node numbering
ReachAttachedToNode(:,3:) = -1_Lng  ! initialize
ReachAttachedToNode(:,1:2)= 0_Lng   ! initialize, the first col will be used to count the number of
                                    ! reaches attached to this node.
counter_reach = 0_Lng               ! this counter counts n. of reaches that shared between ranks

write(*,       *) " Analyzing the partitioned network ... "
write(FileInfo,*) " Analyzing the partitioned network ... "


! Calculating total number of nodes on each rank
  do i_node = 1, Geometry%Base_Geometry%NoNodes
    NNodesOnRanks(this%part(i_node)) = NNodesOnRanks(this%part(i_node)) + 1_Lng
    LocalNodeNumbering(i_node) = NNodesOnRanks(this%part(i_node))
  end do

  do i_reach = 1_Lng, Geometry%Base_Geometry%NoReaches

    NodeI = Geometry%network(i_reach)%ReachNodes(1)
    NodeII= Geometry%network(i_reach)%ReachNodes(2)

    this%ReachPartition(i_reach,1) = this%part(NodeI)
    this%ReachPartition(i_reach,2) = this%part(NodeII)

    ! Figuring out the reach number attached to a each node- reach on the downstream of this node
    ReachAttachedToNode(NodeI, 1) = ReachAttachedToNode(NodeI, 1) + 1_Lng
    ReachAttachedToNode(NodeI, ReachAttachedToNode(NodeI, 1) + 2_Lng)  = i_reach

    ! Figuring out the reach number attached to a each node- reach on the upstream of this node
    ReachAttachedToNode(NodeII, 2) = ReachAttachedToNode(NodeII, 2 ) + 1_Lng
    ReachAttachedToNode(NodeII, ReachAttachedToNode(NodeII, 2) + 6_Lng)  = i_reach

    ! The case that both nodes are on the same rank, i.e. the entire reach is one rank
    if (this%ReachPartition(i_reach,1) == this%ReachPartition(i_reach,2)) then

      NReachOnRanks(this%ReachPartition(i_reach,1)) = &
                                              NReachOnRanks(this%ReachPartition(i_reach,1)) + 1_Lng

      ! no cells from this reach on this rank (the entire reach on one rank)
      this%ReachPartition(i_reach,3) = Discretization%DiscretizedReach(i_reach)%NCells_reach
      this%ReachPartition(i_reach,4) = 0

      ! Local reach numbering on the rank which the reach belongs to.
      this%ReachPartition(i_reach,5) = NReachOnRanks(this%ReachPartition(i_reach,1))
      this%ReachPartition(i_reach,6) = NReachOnRanks(this%ReachPartition(i_reach,1))

      chunk(this%part(NodeI),2) = chunk(this%part(NodeI),2) + this%ReachPartition(i_reach,3)

    ! The case that the reach seats on two different ranks
    else

      counter_reach = counter_reach + 1_Lng

      NReachOnRanks(this%ReachPartition(i_reach,1)) = &
                                              NReachOnRanks(this%ReachPartition(i_reach,1)) + 1_Lng
      NReachOnRanks(this%ReachPartition(i_reach,2)) = &
                                              NReachOnRanks(this%ReachPartition(i_reach,2)) + 1_Lng

      ! Local reach numberings on the ranks which the reach belongs to.
      this%ReachPartition(i_reach,5) = NReachOnRanks(this%ReachPartition(i_reach,1))
      this%ReachPartition(i_reach,6) = NReachOnRanks(this%ReachPartition(i_reach,2))

        if (mod(Discretization%DiscretizedReach(i_reach)%NCells_reach,2)==0 ) then
          ! even no. of cells in this reach:
          tempCell = Discretization%DiscretizedReach(i_reach)%NCells_reach /2_Lng

          ! no. of cells from this reach on this rank
          ! (upstream is on one rank and downstream is on another)
          this%ReachPartition(i_reach,3) = tempCell
          this%ReachPartition(i_reach,4) = tempCell

          chunk(this%part(NodeI), 3) = chunk(this%part(NodeI), 3) + tempCell
          chunk(this%part(NodeII),3) = chunk(this%part(NodeII),3) + tempCell
        else
          ! odd no. of cells in this reach
          tempCell = (Discretization%DiscretizedReach(i_reach)%NCells_reach -1_Lng)/2_Lng

          ! no. of cells from this reach on this rank
          ! (upstream is on one rank and downstream is on another)
          this%ReachPartition(i_reach,3) = tempCell
          this%ReachPartition(i_reach,4) = tempCell+1_Lng

          chunk(this%part(NodeI),3)  = chunk(this%part(NodeI), 3) + tempCell
          chunk(this%part(NodeII),3) = chunk(this%part(NodeII),3) + tempCell+1_Lng
        end if
    end if
  end do

! no. of cells on each rank, saved in col 4.
chunk(:,4) = chunk(:,2) + chunk(:,3)

  ! Checks
  ! check the number of reaches attached to each node. Now, the assumption is that there are at
  ! most only two upstream reaches and one downstream reach.
  do i_node = 1, Geometry%Base_Geometry%NoNodes



    if (ReachAttachedToNode(i_node, 2) > 2_Lng ) then
      write(*, fmt="('Input error: At the time, the assumption is that there are only two &
                      upstream reaches')")

      write(*, fmt="(' Node: ', I10, ' has:', I5, ' upstream reaches.' )")  &
                                                            i_node,  ReachAttachedToNode(i_node, 2)
      stop
    end if
    if (ReachAttachedToNode(i_node, 1) > 1_Lng ) then
      write(*, fmt="('Input error: At the time, the assumption is that there are only one &
                      downstream reach.')")
      write(*, fmt="(' Node: ', I10, ' has:', I5, ' downstream reaches.' )") &
                                                            i_node,  ReachAttachedToNode(i_node, 1)
      stop
    end if
  end do


  if ( sum(NReachOnRanks)-counter_reach /= Geometry%Base_Geometry%NoReaches ) then
    call errorMessage(Geometry%Base_Geometry%NoReaches, sum(NReachOnRanks))
  end if

  do i_reach = 1_Lng, Geometry%Base_Geometry%NoReaches
    write(*,fmt="(5I4)")i_reach, this%ReachPartition(i_reach,1), this%ReachPartition(i_reach,2), &
                                 this%ReachPartition(i_reach,3), this%ReachPartition(i_reach,4)
  end do

  if ( sum(NNodesOnRanks) /= Geometry%Base_Geometry%NoNodes ) then
    call errorMessage(Geometry%Base_Geometry%NoReaches, sum(NNodesOnRanks))
  end if


! We develop an optimization problem here to equalize the no. of cells on each rank.
!Balanced_load = .true.
!  while (Balanced_load) do
!    Balanced_load = .false.
!  end do

TotalCellCounter = 0_Lng
! - printing out the partitioned data -------------------------------------------------------------
  On_Partitions: do i_rank = 1_Shrt, Geometry%Base_Geometry%size
    write(*,*)
    write(*,        fmt="(A,I10)") " Creating the input file for process no.: ", i_rank-1_Shrt
    write(FileInfo, fmt="(A,I10)") " Creating the input file for process no.: ", i_rank-1_Shrt

    ! Opening the output files.
    write(*,       *) " Creating input files ..."
    write(FileInfo,*) " Creating input files ..."

    ! Directories for the input files <modify>
    ! open file for this partition
    write(IndexRank, *) i_rank - 1_Shrt ! Converts Rank to Character format for the file Name

    UnFile = FilePartition
    open(unit=UnFile, &
         file=trim(ModelInfo%ModelName)//'_s'//&
         trim(adjustL(Geometry%Base_Geometry%IndexSize))//'_p'//trim(adjustL(IndexRank))//'.par', &
         Err=1001, iostat=IO_File, access='sequential', action='write', asynchronous='no', &
         blank='NULL', blocksize=0, defaultfile=trim(ModelInfo%InputDir), dispose='keep', &
         form='formatted', position='asis', status='replace')

    ! Writing the total number of cells in each partition
    UnFile = FilePartition
    write(unit=UnFile, fmt="(4I23)", advance='yes', asynchronous='no', iostat=IO_write, err=1006) &
                   chunk(i_rank,4),        &   ! Total number of cells on this rank
                   NReachOnRanks(i_rank),  &   ! Total number of reaches on this rank
                   NNodesOnRanks(i_rank),  &   ! Total number of nodes on this rank
                   Geometry%Base_Geometry%NoReaches  ! Total Num Of Cells In The Network

    CellCounter = 0_Lng ! To make sure that we count all the cell numbers in each rank
    ReachCounter= 0_Lng
    UnFile = FilePartition

      On_Reaches: do i_reach = 1_Lng, Geometry%Base_Geometry%NoReaches

        NodeI = Geometry%network(i_reach)%ReachNodes(1) ! upstream node number for this reach
        NodeII= Geometry%network(i_reach)%ReachNodes(2) ! downstream node number for this reach

        ! The rank which the first node of this reach belongs to
        RankNodeI  = this%ReachPartition(i_reach,1)

        ! The rank which the second node of this reach belongs to
        RankNodeII = this%ReachPartition(i_reach,2)

          ! check whether if any nodes on the reach belongs to this partition.
          if (INT4(RankNodeI) /= i_rank .and. INT4(RankNodeII) /= i_rank) cycle

          if (RankNodeI == RankNodeII) then
          ! In this case, both nodes of the reach belong to the same rank, thus, we dedicate all
          ! cells in this reach on this rank.

            RangeCell_I   = 1_Lng
            RangeCell_II  = this%ReachPartition(i_reach,3)
            Communication = -1_Tiny ! no communication with other ranks. The entire rank sits on one rank.
            CommRank      = -1_Tiny ! The rank number that this reach will communicate.
            BCNodeI       = Geometry%BoundaryCondition(NodeI)
            BCNodeII      = Geometry%BoundaryCondition(NodeII)

            UpstreamI  = ReachAttachedToNode(NodeI,  7)
            UpstreamII = ReachAttachedToNode(NodeI,  8)
            Downstream = ReachAttachedToNode(NodeII, 3)

            LocalNodeOfReachI  = LocalNodeNumbering(NodeI)
            LocalNodeOfReachII = LocalNodeNumbering(NodeII)

              ! figuring out the local reach number for the upstream reaches and the downstream reaches of each reach
              ! upstream reach number 1 (ReachLeft)
              if (ReachAttachedToNode(NodeI, 7) == -1_Lng) then  ! There is no upstream reach for this node (a boundary condition node)
                ReachLeft = -1_Lng
              else  ! There is an upstream reach for this junction.
                ReachLeft   = this%ReachPartition(ReachAttachedToNode(NodeI,  7), 5)
              end if

              ! upstream reach number 2 (ReachRight)
              if (ReachAttachedToNode(NodeI, 8) == -1_Lng) then  ! There is no upstream reach for this node (a boundary condition node)
                ReachRight = -1_Lng
              else  ! There is an upstream reach for this junction.
                ReachRight = this%ReachPartition(ReachAttachedToNode(NodeI,  8), 5)
              end if

              ! downstream reach (ReachBottom)
              if (ReachAttachedToNode(NodeII, 3) == -1_Lng) then  ! There is no downstream reach for this node (a boundary condition node)
                ReachBottom = -1_Lng
              else  ! There is a downstream reach for this junction.
                ReachBottom = this%ReachPartition(ReachAttachedToNode(NodeII,  3), 6)
              end if

          else if (RankNodeI /= RankNodeII) then
          ! In this case, each node of this reach belong to two different ranks, thus, we dedicate
          ! the cells of this ranch on two different ranks.

          ! Next, we need to find out which part of this reach belongs to this rank
            if ( INT4(RankNodeI) == i_rank) then
              RangeCell_I   = 1_Lng
              RangeCell_II  = this%ReachPartition(i_reach,3)
              Communication = 1_Tiny ! indicates that we need to communicate with the rank that
                                     ! holds the lower part of the reach (downstream).
              CommRank = RankNodeII
              BCNodeI       = Geometry%BoundaryCondition(RankNodeI)
              BCNodeII      = -1_Tiny

              UpstreamI  = ReachAttachedToNode(NodeI,  7)
              UpstreamII = ReachAttachedToNode(NodeI,  8)
              Downstream = -1_Lng

              LocalNodeOfReachI  = LocalNodeNumbering(NodeI)
              LocalNodeOfReachII = -1_Lng

                ! figuring out the local reach number for the upstream reaches and the downstream reaches of each reach
                ! upstream reach number 1 (ReachLeft)
                if (ReachAttachedToNode(NodeI, 7) == -1_Lng) then  ! There is no upstream reach for this node (a boundary condition node)
                  ReachLeft = -1_Lng
                else  ! There is an upstream reach for this junction.
                  ReachLeft   = this%ReachPartition(ReachAttachedToNode(NodeI,  7), 5)
                end if

                ! upstream reach number 2 (ReachRight)
                if (ReachAttachedToNode(NodeI, 8) == -1_Lng) then  ! There is no upstream reach for this node (a boundary condition node)
                  ReachRight = -1_Lng
                else  ! There is an upstream reach for this junction.
                  ReachRight = this%ReachPartition(ReachAttachedToNode(NodeI,  8), 5)
                end if

              ! downstream reach (ReachBottom)
              ReachBottom = -1_Lng

            else if ( INT4(RankNodeII) == i_rank) then
              RangeCell_I   = this%ReachPartition(i_reach,3) + 1_Lng
              RangeCell_II  = this%ReachPartition(i_reach,3)+this%ReachPartition(i_reach,4)
              Communication = 2_Tiny ! indicates that we need to communicate with the rank that
                                     ! holds the upper part of the reach (upstream).
              CommRank      = RankNodeI
              BCNodeI       = -1_Tiny
              BCNodeII      = Geometry%BoundaryCondition(RankNodeII)

              UpstreamI  = -1_Lng
              UpstreamII = -1_Lng
              Downstream = ReachAttachedToNode(NodeII, 3)

              LocalNodeOfReachI  = -1_Lng
              LocalNodeOfReachII = LocalNodeNumbering(NodeII)

              ! figuring out the local reach number for the upstream reaches and the downstream reaches of each reach
              ! upstream reach number 1 (ReachLeft)
              ReachLeft   = -1_Lng

              ! upstream reach number 2 (ReachRight)
              ReachRight = -1_Lng

                ! downstream reach (ReachBottom)
                if (ReachAttachedToNode(NodeII, 3) == -1_Lng) then  ! There is no downstream reach for this node (a boundary condition node)
                  ReachBottom = -1_Lng
                else  ! There is a downstream reach for this junction.
                  ReachBottom = this%ReachPartition(ReachAttachedToNode(NodeII,  3), 6)
                end if

            end if
          end if

        ReachCounter = ReachCounter + 1_Lng

        write(unit=UnFile, fmt="(14I12, 6F35.20)", advance='yes', asynchronous='no', &
              iostat=IO_write, err=1006)                                            &
              i_reach,          & ! global reach number, before partitioning
              Communication,    & ! indicates whether we will communicate with other ranks for this reach or not.
              CommRank,         & ! if there is communication with other rank for this reach, this var indicates the rank number to which we need to communicate
              LocalNodeOfReachI, LocalNodeOfReachII, & ! Local node numbers attached to this reach
              BCNodeI, BCNodeII, & ! indicates the type of the boundary conditions of the two nodes attached to this reach.
              RangeCell_II - RangeCell_I + 1_Lng,                                   & ! total no. cells

              UpstreamI, UpstreamII,        & ! the upstream reaches, global numbering
              Downstream,                   & ! the downstream reach, global numbering


              ReachLeft, ReachRight, ReachBottom,                                   & ! local reach numbering of upstream and downstream reaches of this particular reach.

              Discretization%DiscretizedReach(i_reach)%ReachManning,                &
              Discretization%DiscretizedReach(i_reach)%ReachWidthCell,              &
              Discretization%DiscretizedReach(i_reach)%CellPorjectionLength,        &

              Geometry%Q_Up(NodeI),                                                 & ! the discharge, if this upstream node is an inlet boundary condition,
              Geometry%network(i_reach)%CntrlV,                                     & ! the initial control volume
              Geometry%network(i_reach)%CntrlV_ratio

          do i_cells = RangeCell_I, RangeCell_II

            CellCounter = CellCounter + 1_Lng
            write(unit=UnFile, fmt="(6F35.20)", &
                  advance='yes', asynchronous='no', iostat=IO_write, err=1006)       &
                  Discretization%DiscretizedReach(i_reach)%LengthCell    (i_cells),  &
                  Discretization%DiscretizedReach(i_reach)%CellSlope     (i_cells),  &
                  Discretization%DiscretizedReach(i_reach)%InterfaceSlope(i_cells),  &
                  Discretization%DiscretizedReach(i_reach)%ZCell         (i_cells),  &
                  Discretization%DiscretizedReach(i_reach)%YCell         (i_cells),  &
                  Discretization%DiscretizedReach(i_reach)%XCell         (i_cells)
          end do
        write(unit=UnFile, fmt="(F35.20)", &
              advance='yes', asynchronous='no', iostat=IO_write, err=1006) &
              Discretization%DiscretizedReach(i_reach)%InterfaceSlope(i_cells+1_Lng)

        write(*,        fmt="(' -create the discretized domain file for visualization ... ')")
        write(FileInfo, fmt="(' -create the discretized domain file for visualization ... ')")

        ! Print the discretized domain (cell centers) for all reaches in the domain file for visualization in Python
        allocate(Plot_domain_1D_tp(RangeCell_II - RangeCell_I+1) :: Plot, stat=ERR_Alloc)
        if (ERR_Alloc /= 0) call error_in_allocation(ERR_Alloc)

        ! Filling the coordinates for plot
        Plot%XCoor(:)      = Discretization%DiscretizedReach(i_reach)%XCell(RangeCell_I:RangeCell_II)
        Plot%YCoor(:)      = Discretization%DiscretizedReach(i_reach)%YCell(RangeCell_I:RangeCell_II)
        Plot%ZCoor(:)      = Discretization%DiscretizedReach(i_reach)%ZCell(RangeCell_I:RangeCell_II)
        Plot%CellSlope(:)  = Discretization%DiscretizedReach(i_reach)%CellSlope(RangeCell_I:RangeCell_II)
        Plot%IndexSize = Geometry%Base_Geometry%IndexSize
        Plot%IndexRank = IndexRank
        write(IndexReach, *) ReachCounter ! Converts Rank to Character format for the file Name
        Plot%IndexReach = IndexReach

        call Plot%plot(ModelInfo)
        deallocate(Plot)

      end do On_Reaches

    TotalCellCounter = TotalCellCounter + CellCounter

      ! checks
      if (ReachCounter /= NReachOnRanks(i_rank)) then
        call error_in_reaches_on_each_rank(ReachCounter, NReachOnRanks(i_rank))
      end if

    ! - Closing the input file for this partition -------------------------------------------------
    write(*,        *) " Closing the input file ... "
    write(FileInfo, *) " Closing the input file ... "

    UnFile =  FilePartition
    close(unit=UnFile, status="keep", err=1002, iostat=IO_File)

  end do On_Partitions

  if (TotalCellCounter/= Discretization%NCells) then
    write(*,       *) "Fatal error! Mismatch in the number of cells. &
                       Check the partitioner subroutine.", TotalCellCounter, Discretization%NCells
    write(FileInfo,*) "Fatal error! Mismatch in the number of cells. &
                       Check the partitioner subroutine.", TotalCellCounter, Discretization%NCells
    write(*, Fmt_FL); write(FileInfo, Fmt_FL);
    write(*, Fmt_end); read(*,*); stop;
  end if


! writing the network for the case that we have only one partition (for debugging purposes).
else if (Geometry%Base_Geometry%size == 1_Shrt) then

write(*,*)
write(*,        fmt="(A,I10)") " Creating the input file for process no.: ", 0_Shrt
write(FileInfo, fmt="(A,I10)") " Creating the input file for process no.: ", 0_Shrt

! Directories for the input files <modify>
! open file for this partition
write(IndexRank, *) 0_Shrt ! Converts Rank to Character format for the file Name

UnFile = FilePartition
open(unit=UnFile, &
     file=trim(ModelInfo%ModelName)//'_s'//&
     trim(adjustL(Geometry%Base_Geometry%IndexSize))//'_p'//trim(adjustL(IndexRank))//'.par', &
     Err=1001, iostat=IO_File, access='sequential', action='write', asynchronous='no', &
     blank='NULL', blocksize=0, defaultfile=trim(ModelInfo%InputDir), dispose='keep', &
     form='formatted', position='asis', status='replace')

! Writing the total number of cells in each partition
UnFile = FilePartition
write(unit=UnFile, fmt="(2I23)", advance='yes', asynchronous='no', iostat=IO_write, err=1006) &
                                  Discretization%NCells, Geometry%Base_Geometry%NoReaches, &
                                  Geometry%Base_Geometry%NoReaches, Geometry%Base_Geometry%NoNodes

CellCounter = 0_Lng ! To make sure that we count all the cell numbers in each rank
ReachCounter= 0_Lng
UnFile = FilePartition

  do i_reach = 1_Lng, Geometry%Base_Geometry%NoReaches

    ! The rank which the first node of this reach belongs to
    RankNodeI  = Geometry%network(i_reach)%ReachNodes(1)

    ! The rank which the second node of this reach belongs to
    RankNodeII = Geometry%network(i_reach)%ReachNodes(2)

    RangeCell_I   = 1_Lng
    RangeCell_II  = Discretization%DiscretizedReach(i_reach)%NCells_reach
    Communication = -1_Tiny
    CommRank      = -1_Tiny
    BCNodeI       = Geometry%BoundaryCondition(RankNodeI)
    BCNodeII      = Geometry%BoundaryCondition(RankNodeII)

    ReachCounter= ReachCounter + 1_Lng

    write(unit=UnFile, fmt="(6I12, 3F35.20)", advance='yes', asynchronous='no', &
          iostat=IO_write, err=1006) &
          i_reach, Communication, CommRank, BCNodeI, BCNodeII, &
          RangeCell_II, &
          Discretization%DiscretizedReach(i_reach)%ReachManning, &
          Discretization%DiscretizedReach(i_reach)%ReachWidthCell, &
          Discretization%DiscretizedReach(i_reach)%CellPorjectionLength

      do i_cells = RangeCell_I, RangeCell_II

        CellCounter = CellCounter + 1_Lng
        write(unit=UnFile, fmt="(6F35.20)", &
              advance='yes', asynchronous='no', iostat=IO_write, err=1006)       &
              Discretization%DiscretizedReach(i_reach)%LengthCell    (i_cells),  &
              Discretization%DiscretizedReach(i_reach)%CellSlope     (i_cells),  &
              Discretization%DiscretizedReach(i_reach)%InterfaceSlope(i_cells),  &
              Discretization%DiscretizedReach(i_reach)%ZCell         (i_cells),  &
              Discretization%DiscretizedReach(i_reach)%YCell         (i_cells),  &
              Discretization%DiscretizedReach(i_reach)%XCell         (i_cells)
      end do
    write(unit=UnFile, fmt="(F35.20)", &
          advance='yes', asynchronous='no', iostat=IO_write, err=1006) &
          Discretization%DiscretizedReach(i_reach)%InterfaceSlope(i_cells+1_Lng)
  end do

TotalCellCounter = TotalCellCounter + CellCounter

! - Closing the input file for this partition -------------------------------------------------
write(*,        *) " Closing the input file ... "
write(FileInfo, *) " Closing the input file ... "

UnFile =  FilePartition
close(unit=UnFile, status="keep", err=1002, iostat=IO_File)

end if

write(*,       *) " Partitioning conducted successfully."
write(FileInfo,*) " Partitioning conducted successfully."

write(*,       *) " end subroutine < Network_Partitioner_Sub >"
write(FileInfo,*) " end subroutine < Network_Partitioner_Sub >"
return

! Errors ==========================================================================================
! Opening statement Errors
1001 call errorMessage(UnFile, IO_File)

! Close statement Errors
1002 call error_in_closing_a_file(UnFile, IO_File)

! write statement errors
1006 call error_in_writing(UnFile, IO_write)

end subroutine Network_Partitioner_Sub

end module Network_Partitioner_mod
