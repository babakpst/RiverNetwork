
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
!
! File version $Id $
!
! Last update: 06/25/2018
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
use Discretization_mod, only: DiscretizedNetwork_tp
use Input_mod,          only: Input_Data_tp
use Model_mod,          only: Geometry_tp

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


! This type contains all the variables required to partition a graph using METIS version 5.1.0.
! We define the required variables as pointers, because it is described in the METIS manual. Using
! non-pointer variables is also possible, but, to have more control over the METIS options, we
! prefer pointers.
type METIS_var5_tp
  ! The number of vertices in the graph= NoNodes in the network
  integer(kind=Lng), pointer :: nvtxs => null

  ! The number of balancing constraints.
  ! ncon is discussed at the beginning of page 10 of the manual.
  integer(kind=Lng), pointer :: ncon => null

  ! The adjacency structure of the graph as described in Section 5.5. The size of array is nvtxs+1
  integer(kind=Lng), pointer, dimension(:) :: xadj => null

  ! The adjacency structure of the graph as described in Section 5.5.
  ! The size of this array is "2*(number of edges(=no or reaches))"
  integer(kind=Lng), pointer, dimension(:) :: adjncy => null

  ! The weights of the vertices as described in Section 5.5.
  ! This array stays null in our case. The size of this array is nvtxs * ncon
  integer(kind=Lng), pointer, dimension(:) :: vwgt => null

  ! The size of the vertices for computing the total communication volume as described in
  ! Section 5.7. The size of this array is nvtxs
  integer(kind=Lng), pointer, dimension(:) :: vsize => null

  ! The weights of the edges as described in Section 5.5. The size of this array is 2 * m,
  ! where m is the total number of edges.
  integer(kind=Lng), pointer, dimension(:) :: adjwgt => null

  ! The number of parts to partition the graph
  integer(kind=Lng), pointer,              :: nparts => null

  ! specifies the desired weight for each partition and constraint.
  ! The size of the array nparts×ncon.
  integer(kind=Lng), pointer, dimension(:) :: tpwgts  => null

  ! This is an array of size ncon that specifies the allowed load imbalance tolerance
  ! for each constraint.
  integer(kind=Lng), pointer, dimension(:) :: ubvec => null

  ! see page 21.
  integer(kind=Lng), pointer, dimension(:) :: options => null

  ! Upon successful completion, this variable stores the edge-cut or the total communication volume
  ! of the partitioning solution. The value returned depends on the partitioning’s objective
  ! function.
  integer(kind=Lng), pointer               :: objval => null

  ! This is a vector of "size nvtxs" that upon successful completion stores the partition
  ! vector of the graph. The numbering of this vector starts from either 0 or 1, depending on
  ! the value of options[METIS OPTION NUMBERING].
  integer(kind=Lng), pointer, dimension(:) :: part => null
end type METIS_var5_tp

! This type contains all the variables required to partition a graph using METIS version 4.0.0
! We define the required variables as pointers, because it is described in the METIS manual. Using
! non-pointer variables is also possible, but, to have more control over the METIS options, we
! prefer pointers.
type METIS_var4_tp

  ! The number of vertices in the graph= NoNodes in the network
  integer(kind=Lng), pointer :: n => null

  ! The adjacency structure of the graph as described in Section 5.1. The size of array is n+1
  integer(kind=Lng), pointer, dimension(:) :: xadj => null

  ! The adjacency structure of the graph as described in Section 5.1.
  ! The size of this array is "2*(number of edges(=no or reaches))"
  integer(kind=Lng), pointer, dimension(:) :: adjncy => null

  ! The weights of the vertices as described in Section 5.1.
  ! This array stays null in our case. The size of this array is nvtxs * ncon
  integer(kind=Lng), pointer, dimension(:) :: vwgt => null

  ! The weights of the edges as described in Section 5.5. The size of this array is 2 * m,
  ! where m is the total number of edges.
  integer(kind=Lng), pointer, dimension(:) :: adjwgt => null

  ! Used the indicate if the graph is weighted. (see page 22 of manual of version 4).
  ! wgtflags can take the following values:
  ! 1 weights on the edges only (vwgts = NULL)
  integer(kind=Lng), pointer               :: wgtflag => null

  ! Indicated the C or Fortran style of numbering: 0: C or 1: fortran
  integer(kind=Lng), pointer               :: numflag => null

  ! The number of parts to partition the graph
  integer(kind=Lng), pointer,              :: nparts => null

  ! see page 22. Use options[0]=0.
  integer(kind=Lng), pointer, dimension(:) :: options => null

  ! The number of edges that are cut by the partition. In our case, this indicates the total number
  ! of communication between the ranks.
  integer(kind=Lng), pointer               :: edgecut => null

  ! This is a vector of "size nvtxs" that upon successful completion stores the partition
  ! vector of the graph. The numbering of this vector starts from either 0 or 1, depending on
  ! the value of options[METIS OPTION NUMBERING].
  integer(kind=Lng), pointer, dimension(:) :: part => null

end type METIS_var4_tp


type partitioner_tp(edges, nodes)
  integer(kind=Lng), len :: edges  ! number of edges in the network
  integer(kind=Lng), len :: nodes  ! number of nodes in the network

  integer                :: ncon          ! Temp variable for graph partitioning.
  integer                :: wgtflag
  integer                :: numflag
  integer, dimension(0:5):: options4
  integer, dimension(METIS_NOPTIONS) :: options5

  integer(kind=Lng), dimension(edges,4) :: ReachPartition

  integer(kind=Lng), dimension(nodges+1), target :: xadj_target
  integer(kind=Lng), dimension(2*edges),  target :: adjncy_target
  integer(kind=Lng), dimension(2*edges),  target :: adjwgt_target

  type(METIS_var4_tp) :: METIS4
  type(METIS_var5_tp) :: METIS5
  contains
    generic :: Partition => Network_Partitioner_Sub

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
! V2.00: 06/21/2018 - Modifying the subroutineDiscretization to partition a network, use METIS graph partitioner
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

! Libraries =======================================================================================

! User defined modules ============================================================================

implicit none

! Global variables ================================================================================

! - types -----------------------------------------------------------------------------------------
type(Input_Data_tp), intent(In) :: ModelInfo ! Holds info. (name, dir, output dir) of the model
type(Geometry_tp),   intent(In) :: Geometry  ! Holds the geometry of the network
type(DiscretizedNetwork_tp), intent(In) :: Discretization ! Holds the discretized network

class(partitioner_tp) :: this ! defining the variables to partition a network using METIS

! Local variables =================================================================================
! - integer variables -----------------------------------------------------------------------------
integer(kind=Smll) :: UnFile        ! Holds Unit of a file for error message
integer(kind=Smll) :: IO_File       ! For IOSTAT: Input Output Status in OPEN command
integer(kind=Smll) :: IO_write      ! Used for IOSTAT: Input/Output Status in the write command

integer(kind=Shrt) :: i_partition   ! Loop index for the number of processes/ranks
integer(kind=Shrt) :: i             ! Loop index
integer(kind=Shrt) :: remainder     ! Temp var to see the approximate number of cells in each rank

integer(kind=Lng)  :: counter       ! Counter for cells
integer(kind=Lng)  :: i_cells       ! Loop index over cells
integer(kind=Lng)  :: i_reach       ! Loop index over reaches
integer(kind=Lng)  :: i_node        ! Loop index over nodes
integer(kind=Lng)  :: NodeI, NodeII ! Temp var to hold node number of each reach
integer(kind=Lng)  :: tempCell      ! Temp var to hold no. of cells of each rank for a shared reach
integer(kind=Lng)  :: Weights       ! Weight of each edge (= no. cells in the edge= reach)
integer(kind=Lng)  :: NodeLocation  ! Temp var to hold the location of adjacent nodes in the graph

! - integer Arrays --------------------------------------------------------------------------------
integer(kind=Lng), dimension (Geometry%size,4)  :: chunk   ! share of the domain for each rank
                         ! col 1: ideal chunk size
                         ! col 2: certain cells, from reaches with both nodes on this rank
                         ! col 3: unsure cells, from ranks with two nodes on two different ranks
                         ! col 4: summation of cols 2 and 3, the actual no. of cells on this ranks

! - character variables ---------------------------------------------------------------------------
Character(kind = 1, len = 20) :: IndexRank ! Rank no in the Char. fmt to add to the input file Name

logical :: Balanced_load

type(NodeConncetivityArray_tp), allocatable, dimension(:) :: NodeConnectivity ! Linked list
type(NodeConncetivity_tp), pointer :: Temp ! This is a temporary type to create the list

! code ============================================================================================
write(*,        fmt="(A)") " subroutine < Network_Partitioner_Sub >: "
write(FileInfo, fmt="(A)") " subroutine < Network_Partitioner_Sub >: "

! Computing the chunk of each process
write(*,        fmt="(A)") " calculating the average number of cells on each rank ... "
write(FileInfo, fmt="(A)") " calculating the average number of cells on each rank ... "

chunk(:,:) = 0_Lng
remainder = mod(Discretization%NCells, Geometry%Base_Geometry%size)
chunk(:,1)  = (Discretization%NCells - remainder)/Geometry%Base_Geometry%size
  do i = 1_Shrt, remainder
    chunk(i,1) = chunk(i,1) + 1
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

  ! This loop figures out all connectivities between nodes
  do i_reach = 1_Lng, Geometry%Base_Geometry%NoReaches
    NodeI = Geometry%network(i_reach)%ReachNodes(1)
    NodeII= Geometry%network(i_reach)%ReachNodes(2)
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
NodeLocation = 0
counter      = 0
  do i_node = 1_Lng, Geometry%Base_Geometry%NoNodes
    this%xadj_target(i_node) = NodeLocation
    NodeLocation = NodeLocation + NodeConnectivity(i_node)

    Temp => NodeConnectivity(i_node)%head
      do
        if (.not.associated(Temp)) exit
        counter = counter+1_Lng
        this%adjncy_target(counter) = Temp%nodes
        this%adjwgt_target(counter) = Temp%cells
        Temp => Temp%next
      end do

  end do

this%xadj_target(i_node+1) = NodeLocation ! <modify>

  if (counter /= 2_Lng *  Geometry%Base_Geometry%NoReaches) then
    write(*,*) " Something is wrong with the adjacency finder"
    stop
  end if


! - partitioning using METIS ----------------------------------------------------------------------
write(*,        fmt="(A)") " -Graph partitioning using METIS_PartGraphKway... "
write(FileInfo, fmt="(A)") " -Graph partitioning using METIS_PartGraphKway ... "

  if (Geometry%Base_Geometry%METIS_version == 1_Tiny) then ! Partitioning using METIS version 5

    this%METIS5%nvtxs => Geometry%Base_Geometry%NoNodes  ! Setting the number of vertices

    ! setting the number of balancing constraints.
    this%ncon = 0  ! <modify> The other option is to nullify the pointer. Check both cases.
    this%options5(METIS_OPTION_OBJTYPE) = METIS_OBJTYPE_CUT
    !this%options5(METIS_OPTION_NCUTS)   = ?
    this%options5(METIS_OPTION_NUMBERING) = 1

    this%METIS5%ncon   => this%ncon
    this%METIS5%adjncy => this%adjncy_target
    this%METIS5%adjwgt => this%adjwgt_target
    this%METIS5%xadj   => this%xadj_target
    this%METIS5%nparts => Geometry%Base_Geometry%size
    this%METIS5%options=> this%options5

    call METIS_PartGraphKway(this%METIS5%nvtxs,   &
                             this%METIS5%ncon,    &
                             this%METIS5%xadj,    &
                             this%METIS5%adjncy,  &
                             this%METIS5%vwgt,    &
                             this%METIS5%vsize,   &
                             this%METIS5%adjwgt,  &
                             this%METIS5%nparts,  &
                             this%METIS5%tpwgts,  &
                             this%METIS5%ubvec,   &
                             this%METIS5%options, &
                             this%METIS5%objval,  &
                             this%METIS5%part)    &

  else if (Geometry%Base_Geometry%METIS_version == 0_Tiny) then ! Partitioning using METIS ver. 4

    this%wgtflag = 1
    this%numflag = 1
    this%options(:) = 0
    this%METIS4%n       => Geometry%Base_Geometry%NoNodes  ! Setting the number of vertices
    this%METIS4%xadj    => this%xadj_target
    this%METIS4%adjncy  => this%adjncy_target
    this%METIS4%adjwgt  => this%adjwgt_target
    this%METIS4%wgtflag => this%wgtflag
    this%METIS4%numflag => this%numflag
    this%METIS4%nparts  => Geometry%Base_Geometry%size
    this%METIS4%options => this%options4

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
                             this%METIS4%part)    &
  else
    write(*,*) " The requested METIS version does not exist. Please, enter 0 for METIS version 4 &
                 or 1 for METIS version 5 in the --.DataModel file."

    call error_in_the_requested_option()
    stop
  end if

! - analyzing the partitioned graph

! In this loop, we loop over the reaches and we see ranks the two nodes of the reach are belong to.
! If both nodes of a reach belong to one rank, then we devote the entire cells in this reach, to
! this rank. But, if the two nodes of the reach belong to two different ranks, then, initially, we
! divide the cells evenly between the two ranks. In the next step, we will play with the cells
! until we have an even distribution. We would like to distribute the
! cells as even as possible, so that the share of each rank is as close as possible to the number
! we calculated in the beginning of this subroutine, saved in the "chunk" array.
! At the end of this loop, we have a rough estimation of the partitioning and the no. of cells on
! each rank. In the next step, we try to equalize the no. of cells on each reach.
  do i_reach = 1_Lng, Geometry%Base_Geometry%NoReaches
    NodeI = Geometry%network(i_reach)%ReachNodes(1)
    NodeII= Geometry%network(i_reach)%ReachNodes(2)

    this%ReachPartition(i_reach,1) = part(NodeI)
    this%ReachPartition(i_reach,2) = part(NodeII)

    if (this%ReachPartition(i_reach,1) == this%ReachPartition(i_reach,2)) then
      this%ReachPartition(i_reach,3) = Discretization%DiscretizedReach(i_reach)%NCells_reach
      this%ReachPartition(i_reach,4) = 0
      chunk(part(NodeI),2) = chunk(part(NodeI),2) + this%ReachPartition(i_reach,3)
    else

      if (mod(Discretization%DiscretizedReach(i_reach)%NCells_reach,2)==0 ) then
        ! even no. of cells in this reach:
        tempCell = Discretization%DiscretizedReach(i_reach)%NCells_reach /2_Lng
        this%ReachPartition(i_reach,3) = tempCell
        this%ReachPartition(i_reach,4) = tempCell
        chunk(part(NodeI),3)  = chunk(part(NodeI),3) + tempCell
        chunk(part(NodeII),3) = chunk(part(NodeI),3) + tempCell
      else
        ! odd no. of cells in this reach
        tempCell = (Discretization%DiscretizedReach(i_reach)%NCells_reach -1_Lng)/2_Lng
        this%ReachPartition(i_reach,3) = tempCell
        this%ReachPartition(i_reach,4) = tempCell+1_Lng
        chunk(part(NodeI),3)  = chunk(part(NodeI),3) + tempCell
        chunk(part(NodeII),3) = chunk(part(NodeI),3) + tempCell+1_Lng
      end if
    end if

  end do

! no. of cells on each rank, saved in col 4.
chunk(:,4) = chunk(:,2) + chunk(:,3)

! We develop an optimization problem here to equalize the no. of cells on each rank.
!Balanced_load = .true.
!  while (Balanced_load) do
!    Balanced_load = .false.
!  end do

! - printing out the partitioned data -------------------------------------------------------------

counter = 0_Lng

  On_Partitions: do i_partition = 1, Geometry%size
    write(*,*)
    write(*,        fmt="(A,I10)") " Partitioning for process no.: ", i_partition-1_Shrt
    write(FileInfo, fmt="(A,I10)") " Partitioning for process no.: ", i_partition-1_Shrt

    ! Opening the output files.
    write(*,       *) " Creating input files ..."
    write(FileInfo,*) " Creating input files ..."

    ! Directories for the input files <modify>
    ! open file for this partition
    write(IndexRank, *) i_partition - 1_Shrt ! Converts Rank to Character format for the file Name

    UnFile = FilePartition
    open(unit=UnFile, &
         file=trim(ModelInfo%ModelName)//'_s'//&
              trim(adjustL(Geometry%IndexSize))//'_p'//trim(adjustL(IndexRank))//'.par', &
         Err=1001, iostat=IO_File, access='sequential', action='write', asynchronous='no', &
         blank='NULL', blocksize=0, defaultfile=trim(ModelInfo%InputDir), dispose='keep', &
         form='formatted', position='asis', status='replace')




    ! Writing the total number of cells in each partition
    UnFile = FilePartition
    write(unit=UnFile, fmt="(I23)", advance='yes', asynchronous='no', &
          iostat=IO_write, err=1006) chunk(i_partition,4)

    UnFile = FilePartition

      do i_reach = 1_Lng, Geometry%Base_Geometry%NoReaches
        this%ReachPartition()
      end do







      do i_cells = 1_Lng, chunk(i_partition)

        counter = counter + 1_Lng
        write(unit=UnFile, fmt="(8F35.20)", &
              advance='yes', asynchronous='no', iostat=IO_write, err=1006) &
              Discretization%LengthCell (counter,1),                       &
              Discretization%LengthCell (counter,2),                       &
              Discretization%CellSlope  (counter),                         &
              Discretization%ZCell      (counter),                         &
              Discretization%ManningCell(counter),                         &
              Discretization%WidthCell  (counter),                         &
              Discretization%XCell     (counter),                          &
              Discretization%InterfaceSlope (counter)

              !Discretization%ZFull      (counter*2_Lng-1_Lng),             &
              !Discretization%ZFull      (counter*2_Lng),                   &
              !Discretization%XFull     (counter*2_Lng-1_Lng),             &
              !Discretization%XFull     (counter*2_Lng      )
      end do
    write(unit=UnFile, fmt="(F35.20)", &
          advance='yes', asynchronous='no', iostat=IO_write, err=1006) &
          Discretization%InterfaceSlope(counter+1_Lng)

    ! - Closing the input file for this partition -------------------------------------------------
    write(*,        *) " Closing the input file ... "
    write(FileInfo, *) " Closing the input file ... "

    UnFile =  FilePartition
    close(unit=UnFile, status="keep", err=1002, iostat=IO_File)

  end do On_Partitions

  if (counter/= Discretization%NCells) then
    write(*,       *) "Fatal error! Mismatch in the number of cells. &
                       Check the partitioner subroutine.", counter, Discretization%NCells
    write(FileInfo,*) "Fatal error! Mismatch in the number of cells. &
                       Check the partitioner subroutine.", counter, Discretization%NCells
    write(*, Fmt_FL); write(FileInfo, Fmt_FL);
    write(*, Fmt_end); read(*,*); stop;
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
