
!##################################################################################################
! Purpose: This module solves the shallow water equations using a combination of upwind method with
!          Lax-Wendroff method, using a limiter. This module contains all other requirements,
!          limiter, boundary conditions, etc.
!
! Developed by: Babak Poursartip
! Supervised by: Clint Dawson
!
! The Institute for Computational Engineering and Sciences (ICES)
! The University of Texas at Austin
!
! ================================ V E R S I O N ==================================================
! V0.00: 03/15/2018 - File initiated.
! V0.01: 03/24/2018 - Initiated: Compiled without error for the first time.
! V1.00: 03/27/2018 - Compiled without error for the first time.
! V2.00: 04/19/2018 - Parallel, OMP
! V2.10: 04/24/2018 - Parallel, performance
! V2.20: 05/09/2018 - Parallel, performance
! V3.00: 05/16/2018 - MPI Parallel
! V3.10: 05/25/2018 - Modifications
! V3.20: 05/29/2018 - Modifications
! V4.00: 08/01/2018 - Solving network
! V4.00: 02/14/2019 - adding paraview module for visualization
!
! File version $Id $
!
! Last update: 02/14/2019
!
! ================================ S U B R O U T I N E ============================================
! - Solver_1D_with_Limiter_sub: Solves the 1D shallow water equation, with limiter.
! - Impose_BC_1D_up_sub: Imposes boundary conditions on the 1D model at the upstream
! - Impose_BC_1D_dw_sub: Imposes boundary conditions on the 1D model at the downstream
! - Limiters_sub: Contains various limiters.
! - Jacobian_sub: computes the Jacobian matrix, Jacobian plus, and Jacobian minus at each cell.
! - Eigenvalues_sub: computes the eigenvalues of a 2x2 matrix.
!
! ================================ F U N C T I O N ================================================
!
! ================================ L O C A L   V A R I A B L E S ==================================
! (Refer to the main code to see the list of imported variables)
!  . . . . . . . . . . . . . . . . Variables . . . . . . . . . . . . . . . . . . . . . . . . . . .
!
!##################################################################################################

module Solver_mod

! Libraries =======================================================================================

! User defined modules ============================================================================
use Parameters_mod
use Results_mod, only: Plot_Results_1D_limiter_tp
use Input_mod
use reading_network_mod,  only: network_tp
use Timer_mod
use messages_and_errors_mod
use ParaviewOutput_mod, only:ResultNetwork_tp

implicit none
private

type LimiterFunc_tp ! contains all variable to compute the limiter value
  integer(kind=Smll) :: limiter_Type=1_smll ! Indicates the type of limiter function

  real(kind=Dbl)     :: phi = 0.0_dbl    ! the value of limiter
  real(kind=Dbl)     :: theta = 0.0_dbl  ! the argument of the limiter

  contains
    procedure LimiterValue => Limiters_sub
end type LimiterFunc_tp

! This type consists of all variables/arrays regarding the Jacobian, used to apply the limiter.
type Jacobian_tp
  integer(kind=Tiny) :: option=1_Tiny ! indicates how to interpolate the Jacobian at the interface:
                                      ! 1: for average on the solution
                                      ! 2: direct average on the Jacobian itself

  real(kind=Dbl),dimension(2,2) :: A=0.0_dbl ! Jacobian matrix at each time step at interface i-1/2
  real(kind=Dbl),dimension(2,2) :: R=0.0_dbl ! eigenvectors at each time step at interface i-1/2
  real(kind=Dbl),dimension(2,2) :: L=0.0_dbl ! eigenvctrs inverse(R^-1) at each timestep
                                             !  at interface i-1/2

  ! Jacobian mtx at each time step and the interface
  real(kind=Dbl),dimension(2,2) :: A_plus=0.0_dbl  ! + eigenvalues at each time step at interface
  real(kind=Dbl),dimension(2,2) :: A_minus=0.0_dbl ! - eigenvalues at each time step at interface
  real(kind=Dbl),dimension(2,2) :: A_abs=0.0_dbl   ! abs eigenvalues at each time step at interface

  type(vector) :: U_up=vector(U=0.0_dbl) ! Holds the solution at the upstream of each cell
  type(vector) :: U_dw=vector(U=0.0_dbl) ! Holds the solution at the downstream of each cell

  type(vector) :: Lambda=vector(U=0.0_dbl)       ! Contains the eigenvalues
  type(vector) :: Lambda_plus=vector(U=0.0_dbl)  ! Contains the positive eigenvalues
  type(vector) :: Lambda_minus=vector(U=0.0_dbl) ! Contains the negative eigenvalues

  contains
    procedure Jacobian => Jacobian_sub
end type Jacobian_tp


! Solution on this rank
type solutionAtTimeSteps
  type(vector), allocatable, dimension(:) :: UU, UN
end type solutionAtTimeSteps


! Contains the parameters for considering the source term within the solution.
type SoureceTerms_tp
  real(kind=Dbl)  :: S_f=0.0_dbl           ! friction slope
  real(kind=Dbl)  :: S_f_interface=0.0_dbl ! friction slope at the interface

  type(vector)  :: S=vector(U=0.0_dbl)            ! source term
  type(vector)  :: S_interface=vector(U=0.0_dbl)  ! source term at the interface

  ! contribution of the source term in updating the solution (see manual)
  type(vector) :: Source_1=vector(U=0.0_dbl)
  type(vector) :: Source_2=vector(U=0.0_dbl)

  real(kind=Dbl), dimension(2,2) :: B=0.0_dbl        ! This is in fact dS / dU
  real(kind=Dbl), dimension(2,2) :: BI=0.0_dbl       ! B inverse, see notes
  real(kind=Dbl), dimension(2,2) :: Identity=0.0_dbl ! Identity matrix
end type SoureceTerms_tp

! Parametrized type is not compatible with OMP
! Contains the parameters for the solution
!type, public :: SolverWithLimiter(NCells)
!  integer(kind=Lng), len :: NCells
!  integer(kind=Lng)      :: Plot_Inc = 100!

!  type(vector), dimension(NCells) :: S     ! Source term
!  type(vector), dimension(-1_Lng:NCells+2_Lng) :: U !  the solution at the current step,
                                            ! the first term holds "h" and the second holds "uh"
!  type(model_tp) :: Discretization ! Contains the discretization of the domain
!  type(AnalysisData_tp)   :: AnalysisInfo   ! Holds information for the analysis
!  type(Input_Data_tp)     :: ModelInfo      ! Holds information for the model

!  contains
!    procedure Solve => Solver_1D_with_Limiter_sub
!    procedure BC => Impose_Boundary_Condition_1D_sub
!end type SolverWithLimiter


! Contains the parameters for the solution
type:: SolverWithLimiter_tp
  type(network_tp)      :: Model         ! Contains the model
  !type(AnalysisData_tp):: AnalysisInfo  ! Holds information for the analysis
  type(Input_Data_tp)   :: ModelInfo     ! Holds information for the model

  contains
    procedure Solve => solve_the_network_sub
end type SolverWithLimiter_tp

public:: SolverWithLimiter_tp

contains

!##################################################################################################
! Purpose: This module solves the shallow water equations using a combination of upwind method with
!          Lax-Wendroff method, using a limiter.
!
! Developed by: Babak Poursartip
! Supervised by: Clint Dawson
!
! The Institute for Computational Engineering and Sciences (ICES)
! The University of Texas at Austin
!
! ================================ V E R S I O N ==================================================
! V0.00: 03/15/2018 - File initiated.
! V0.01: 03/24/2018 - Initiated: Compiled without error for the first time.
! V1.01: 04/24/2018 - Parallel
! V2.00: 05/09/2018 - Performance
! V3.00: 05/16/2018 - MPI
! V4.00: 08/01/2018 - Solving the network
! V4.01: 02/11/2019 - Adding Paraview module
!
! File version $Id $
!
! Last update: 02/11/2019
!
! ================================ L O C A L   V A R I A B L E S ==================================
! (Refer to the main code to see the list of imported variables)
!  . . . . . . . . . . . . . . . . Variables . . . . . . . . . . . . . . . . . . . . . . . . . . .
!
!##################################################################################################

subroutine solve_the_network_sub(this, TotalTime, AnalysisInfo)

! Libraries =======================================================================================
!$ use omp_lib
use MPI

! User defined modules ============================================================================

implicit none

! Global variables ================================================================================

! - types -----------------------------------------------------------------------------------------
class(SolverWithLimiter_tp) :: this
type(Timer_tp):: TotalTime
! Holds information for the analysis
type(AnalysisData_tp) :: AnalysisInfo

! Local variables =================================================================================
! - integer variables -----------------------------------------------------------------------------
!$ integer :: ITS, MTS  ! thread number of number of threads

! MPI parameters
integer :: MPI_err ! <MPI>
integer :: status(MPI_STATUS_SIZE)

integer, dimension(this%Model%NCutsOnRanks) :: tag_sent, tag_recv
integer, dimension(this%Model%NCutsOnRanks) :: request_sent, request_recv

integer(kind=Lng)  :: i_Cell    ! loop index over the Cells
integer(kind=Lng)  :: i_steps   ! loop index over the number steps
integer(kind=Lng)  :: NSteps    ! Total number of steps for time marching
integer(kind=Lng)  :: i_reach   ! loop index over the reaches
integer(kind=Lng)  :: Counter_ReachCut ! We use this var to manage send/receive requests

! The local reach number of the upstream reach attached to the current reach, left
integer(kind=Lng)  :: UpstreamReachNumLeft

! The local reach number of the upstream reach attached to the current reach, right
integer(kind=Lng)  :: UpstreamReachNumRight

! The local reach number of the downstream reach attached to the current reach
integer(kind=Lng)  :: DownstreamReachNum

integer(kind=Lng)  :: NCellsOnTheReach ! holds number of cells on each reach

integer(kind=Smll) :: ERR_Alloc, ERR_DeAlloc ! Allocating and DeAllocating errors

integer(kind=Tiny) :: i_eigen     ! loop index over the eigenvalues (only two in case of 1D SWE)
integer(kind=Tiny) :: i_Interface ! loop index over the interfaces (only two in case of 1D SWE)

! - real variables --------------------------------------------------------------------------------
real(kind=Dbl)     :: dt      ! time step, should be structured/constant in each reach
real(kind=Dbl)     :: dx      ! cell length, should be structured/constant in each reach
real(kind=Dbl)     :: speed   ! characteristic speed, equal to pos./neg. eiqenv. in each interface
real(kind=Dbl)     :: dtdx    ! The ratio dt/dx, used in the final equation
real(kind=Dbl)     :: Coefficient ! take care of the sign of the flux for the high-resolution part

real(kind=Dbl)     :: height   ! height of water at the current cell/time
real(kind=Dbl)     :: velocity ! velocity of water at the current cell/time

real(kind=Dbl)     :: height_interface   ! height of water at the current interface/time
real(kind=Dbl)     :: velocity_interface ! velocity of water at the current interface/time

! - logical variables -----------------------------------------------------------------------------
logical   :: PrintResults

! - type ------------------------------------------------------------------------------------------
type(vector)         :: TempSolution
type(Jacobian_tp)    :: Jacobian          ! Contains the Jacobian and all related items.
type(Jacobian_tp)    :: Jacobian_neighbor ! Contains the Jacobian and all related items.
type(LimiterFunc_tp) :: LimiterFunc       ! Contains the values of the limiter

type(Plot_Results_1D_limiter_tp) :: Results ! in each time step/ all cells

type(SoureceTerms_tp) :: SourceTerms

type(vector) :: alpha                 ! alpha = R^-1 (U_i- U_i-1) See notes for detail.
type(vector) :: alpha_neighbor        ! alpha = R^-1 (U_i- U_i-1) See notes for detail.
type(vector) :: alpha_tilda           ! alpha_tilde = alpha * limiter

type(vector) :: Wave                 ! Wave = alpha * R
type(vector) :: Wave_neighbor        ! Wave = alpha * R
type(vector) :: Wave_tilda           ! Wave = alpha_tilde * R

type(vector) :: F_L ! Contribution of low-resolution method (Upwind) in the solution.
type(vector) :: F_H ! Contribution of high-resolution method (Lax-Wendroff) in the solution.

type(vector) :: Delta_U           ! holds (U_i- U_i-1)

! Items for MPI send and receive messages- for each reach that has been cut, we send/receive the
! solution for two cells
type(vector), dimension(2*this%Model%NCutsOnRanks) :: sent, recv

! solution at time steps n and n+1
type(solutionAtTimeSteps), dimension(this%Model%TotalNumOfReachesOnThisRank) :: Solution

! holds information for visualization with Paraview
type(ResultNetwork_tp):: Paraview

! code ============================================================================================
write(*,       *) " subroutine < solve_the_network_sub >: "
write(FileInfo,*) " subroutine < solve_the_network_sub >: "

! Applying initial conditions:
write(*,       *) " -Applying initial conditions ..."
write(FileInfo,*) " -Applying initial conditions ..."

! <modify>
!!!allocate(Plot_Results_1D_limiter_tp(NCells = this%Model%NCells) :: Results)
!!allocate(Results%U(-1:this%Model%NCells+2),     stat=ERR_Alloc)
!!if (ERR_Alloc /= 0) call error_in_allocation(ERR_Alloc)
!Results%NCells = this%Model%NCells

! Initialization:
NSteps = AnalysisInfo%TotalTime/AnalysisInfo%TimeStep
dt     = AnalysisInfo%TimeStep

! <modify>
!Jacobian%option = 1
!Jacobian_neighbor%option = 1

LimiterFunc%limiter_Type = AnalysisInfo%limiter ! Define what limiter to use in the algorithm
!PrintResults = .true.
PrintResults = .false.
!SourceTerms%Identity(:,:) = 0.0_Dbl
SourceTerms%Identity(1,1) = 1.0_Dbl
SourceTerms%Identity(2,2) = 1.0_Dbl

write(*,       *) " -Allocating the solution ..."
write(FileInfo,*) " -Allocating the solution ..."

! allocating paraview array for visualization with Paraview
allocate(Paraview%ResultReach(this%Model%TotalNumOfReachesOnThisRank),  &
         Paraview%NoCells(this%Model%TotalNumOfReachesOnThisRank), &
         Paraview%NoReachonRanks(this%ModelInfo%size), &
         stat = ERR_Alloc)
if (ERR_Alloc /= 0) call error_in_allocation(ERR_Alloc)

! allocating memory
  do i_reach =1, this%Model%TotalNumOfReachesOnThisRank

    ! allocating the solution in each rank
    allocate(                                                                               &
      Solution(i_reach)%UU(-1_Lng:this%Model%DiscretizedReach(i_reach)%NCells_reach+2_Lng), &
      Solution(i_reach)%UN(-1_Lng:this%Model%DiscretizedReach(i_reach)%NCells_reach+2_Lng), &
                                                                                    stat=ERR_Alloc)
    if (ERR_Alloc /= 0) call error_in_allocation(ERR_Alloc)

    ! allocating paraview arrays for each reach for visualization with Paraview
    allocate(Paraview%ResultReach(i_reach)%U(this%Model%DiscretizedReach(i_reach)%NCells_reach), &
          stat = ERR_Alloc)
    if (ERR_Alloc /= 0) call error_in_allocation(ERR_Alloc)

    ! setting no. cells on each reach of this rank
    Paraview%NoCells(i_reach) = this%Model%DiscretizedReach(i_reach)%NCells_reach

  end do

Counter_ReachCut = 0_Lng

write(*,       *) " -Apply boundary condition to the solution ..."
write(FileInfo,*) " -Apply boundary condition to the solution ..."

tag_recv(:) = 0       ! initialize the tag for MPI send/recv
tag_sent(:) = 0       ! initialize the tag for MPI send/recv
request_recv(:) = 0   ! initialize the tag for MPI send/recv
request_sent(:) = 0   ! initialize the tag for MPI send/recv

! initializing the solution (height+velocity), at time step 0, including the ghost/junction cells.
! The ghost cells are located at the junctions, or the upstream node, or at the downstream node.
  do i_reach =1, this%Model%TotalNumOfReachesOnThisRank

    write(*,       *) " --------Boundary condition for reach: ", i_reach, this%ModelInfo%rank
    write(FileInfo,*) " --------Boundary condition for reach: ", i_reach, this%ModelInfo%rank

    NCellsOnTheReach = this%Model%DiscretizedReach(i_reach)%NCells_reach


    ! initialize the height part of the solution at time-step 0/ except the ghost cells.
    ! This needs to be <modify>ied, if we have a different strategy
    Solution(i_reach)%UU(1:NCellsOnTheReach)%U(1) = this%Model%DiscretizedReach(i_reach)%CntrlV

                                                                             !-this%Model%ZCell(:)
    ! initialize the velocity (uh) part of the solution at time-step 0/ except the ghost cells.
    Solution(i_reach)%UU(:)%U(2) = 0.0_Dbl

  end do

! working on the boundary conditions at the ghost cells
  do i_reach =1, this%Model%TotalNumOfReachesOnThisRank

    write(*,       *) " --------Boundary condition for reach: ", i_reach, this%ModelInfo%rank
    write(FileInfo,*) " --------Boundary condition for reach: ", i_reach, this%ModelInfo%rank

    NCellsOnTheReach = this%Model%DiscretizedReach(i_reach)%NCells_reach

    UpstreamReachNumLeft   = this%Model%DiscretizedReach(i_reach)%UpstreamReaches(1,2)
    UpstreamReachNumRight  = this%Model%DiscretizedReach(i_reach)%UpstreamReaches(2,2)
    DownstreamReachNum     = this%Model%DiscretizedReach(i_reach)%DownstreamReaches(1,2)

      ! working on the ghost cells, or on the junction nodes, for each reach
      if (this%Model%DiscretizedReach(i_reach)%Communication == -1_Tiny) then
        ! no communication with other ranks, the entire reach is on this rank.
        ! There are 2 ghost cells at each ends of this reach, where the nodes are located.

          ! upstream node --
          if (this%Model%DiscretizedReach(i_reach)%BCNodeI == -1_tiny) then
            ! check the correctness of the boundary condition- since the communication is -1, which
            ! means no communication, this upstream and downstream node should be on this rank. As
            ! a result BCNodeI should not be -1.
            write(*,*) " This number should not be equal to -1. Double check the partitioner code."
            write(*,*) " Failed to proceed successfully. Check the solver subroutine!"
            stop
          else if (this%Model%DiscretizedReach(i_reach)%BCNodeI == 0_tiny) then
            ! if the upstream node is a junction, not a BC. In this case, we need to decide about
            ! the node based on the junction modeling.
            call Junction_simulation_flow_combination_upstream( &
            AnalysisInfo%Junction_Model,  &

            this%Model%DiscretizedReach(UpstreamReachNumLeft )%ReachWidthCell, &
            this%Model%DiscretizedReach(UpstreamReachNumRight)%ReachWidthCell, &
            this%Model%DiscretizedReach(i_reach              )%ReachWidthCell, &

            Solution(UpstreamReachNumLeft )%UU(this%Model%DiscretizedReach(UpstreamReachNumLeft )%NCells_reach), &
            Solution(UpstreamReachNumRight)%UU(this%Model%DiscretizedReach(UpstreamReachNumRight)%NCells_reach), &
            Solution(i_reach)%UU(1),   &

            Solution(i_reach)%UU(0),   &
            Solution(i_reach)%UU(-1))

          else if (this%Model%DiscretizedReach(i_reach)%BCNodeI == 1_tiny) then
            ! if the upstream node is a boundary condition/inlet
            call Impose_BC_1D_up_sub(Solution(i_reach)%UU(1)%U(1),                         &
                                     this%Model%DiscretizedReach(i_reach)%Q_Up,            &
                                     this%Model%DiscretizedReach(i_reach)%ReachWidthCell,  &
                                     Solution(i_reach)%UU(-1_Lng), Solution(i_reach)%UU(0_Lng))

          else if (this%Model%DiscretizedReach(i_reach)%BCNodeI == 2_tiny) then
            ! if the upstream node cannot be an outlet boundary condition.
            write(*,*) " This number should not be equal to 2. Double check the partitioner code."
            write(*,*) " Failed to proceed successfully. Check the solver subroutine!"
            stop
          end if

          ! downstream node
          if (this%Model%DiscretizedReach(i_reach)%BCNodeII == -1_tiny) then
            ! check the correctness of the boundary condition- since the communication is -1, which
            ! means no communication, this upstream and downstream node should be on this rank. As
            ! a result BCNodeII should not be -1.
            write(*,*) " This number should not be equal to -1. Double check the partitioner code."
            write(*,*) " Failed to proceed successfully. Check the solver subroutine!"
            stop
          else if (this%Model%DiscretizedReach(i_reach)%BCNodeII == 0_tiny) then
            ! if the upstream node is a junction, no BC. In this case, we need to decide about the
            ! node based on the junction modeling.
            call Junction_simulation_flow_combination_downstream(AnalysisInfo%Junction_Model,   &

                    this%Model%DiscretizedReach(i_reach)%ReachWidthCell,                 &
                    this%Model%DiscretizedReach(DownstreamReachNum)%ReachWidthCell,      &

                    Solution(i_reach)%UU(this%Model%DiscretizedReach(i_reach)%NCells_reach),    &
                    Solution(DownstreamReachNum)%UU(1),                                         &

                    Solution(i_reach)%UU(this%Model%DiscretizedReach(i_reach)%NCells_reach+1),  &
                    Solution(i_reach)%UU(this%Model%DiscretizedReach(i_reach)%NCells_reach+2))

          else if (this%Model%DiscretizedReach(i_reach)%BCNodeII == 1_tiny) then
            ! if the downstream node cannot be an inlet boundary condition
            ! <modify>
            write(*,*) " This number should not be equal to 1. Double check the partitioner code."
            write(*,*) " Failed to proceed successfully. Check the solver subroutine!"
            stop
          else if (this%Model%DiscretizedReach(i_reach)%BCNodeII == 2_tiny) then
            ! if the upstream node is a boundary condition/outlet
            call Impose_BC_1D_dw_sub( &
                  Solution(i_reach)%UU(this%Model%DiscretizedReach(i_reach)%NCells_reach)%U(2),  &
                  AnalysisInfo%h_dw,                                                             &
                  Solution(i_reach)%UU(this%Model%DiscretizedReach(i_reach)%NCells_reach+1_Lng), &
                  Solution(i_reach)%UU(this%Model%DiscretizedReach(i_reach)%NCells_reach+2_Lng))
          end if

      else if (this%Model%DiscretizedReach(i_reach)%Communication == 1_Tiny) then ! upstream half
                                                                   ! of this reach is on the rank
        ! We need to communicate with the rank that holds the downstream of this reach,
        ! the upstream of this reach is on this rank.
        ! The ghost cells at the upstream are at the junction, which is located on this rank.
        ! There is no ghost cell at the downstream, but, we need to communicate with the rank that
        ! holds the downstream of this reach to get the cells information at the bottom.

        ! upstream node
          if (this%Model%DiscretizedReach(i_reach)%BCNodeI == -1_tiny) then
            ! check the correctness of the boundary condition- since the communication is 1, which
            ! means the upstream of this reach is on this rank, thus, the upstream node should be
            ! on this rank. As a result, BCNodeI should not be -1.
            write(*,*) " This number should not be equal to -1. Double check the partitioner code."
            write(*,*) " Failed to proceed successfully. Check the solver subroutine!"
            stop
          else if (this%Model%DiscretizedReach(i_reach)%BCNodeI == 0_tiny) then
            ! if the upstream node is a junction, not a BC. In this case, we need to decide about
            ! the node based on the junction modeling.
            call Junction_simulation_flow_combination_upstream(AnalysisInfo%Junction_Model, &

            this%Model%DiscretizedReach(UpstreamReachNumLeft)%ReachWidthCell,  &
            this%Model%DiscretizedReach(UpstreamReachNumRight)%ReachWidthCell, &
            this%Model%DiscretizedReach(i_reach)%ReachWidthCell,    &

            Solution(UpstreamReachNumLeft)%UU(this%Model%DiscretizedReach(UpstreamReachNumLeft)%NCells_reach),     &
            Solution(UpstreamReachNumRight)%UU(this%Model%DiscretizedReach(UpstreamReachNumRight)%NCells_reach),   &
            Solution(i_reach)%UU(1),                                &

            Solution(i_reach)%UU(0),                                &
            Solution(i_reach)%UU(-1))

          else if (this%Model%DiscretizedReach(i_reach)%BCNodeI == 1_tiny) then
            ! if the upstream node is a boundary condition/inlet
            call Impose_BC_1D_up_sub(Solution(i_reach)%UU(1)%U(1),                        &
                                     this%Model%DiscretizedReach(i_reach)%Q_Up,           &
                                     this%Model%DiscretizedReach(i_reach)%ReachWidthCell, &
                                     Solution(i_reach)%UU(-1_Lng), Solution(i_reach)%UU(0_Lng))

          else if (this%Model%DiscretizedReach(i_reach)%BCNodeI == 2_tiny) then
            ! if the upstream node cannot be an outlet boundary condition
            write(*,*) " This number should not be equal to 2. Double check the partitioner code."
            write(*,*) " Failed to proceed successfully. Check the solver subroutine!"
            stop
          end if

        ! downstream cells
          ! get the solution from the rank that has the downstream
          if (this%Model%DiscretizedReach(i_reach)%BCNodeII /= -1_tiny) then
            ! check the correctness of the boundary condition- since the communication is 1, which
            ! means the upstream of this reach is on this rank, thus, this downstream node
            ! should not be on this rank. As a result, BCNodeII should be -1.
            write(*,*) " This number should be equal to -1. Double check the partitioner code."
            write(*,*) " Failed to proceed successfully. Check the solver subroutine!"
            stop
          end if


          Counter_ReachCut = Counter_ReachCut + 1_Lng

          ! communicate with the node that has the downstream part of this reach.
          ! Sending/Receiving cell info
          ! The downstream of this reach is on another rank
          sent(1+ 2_Lng*(Counter_ReachCut - 1_Lng))%U(:) =  &
                 Solution(i_reach)%UU(this%Model%DiscretizedReach(i_reach)%NCells_reach)%U(:)
          sent(2+ 2_Lng*(Counter_ReachCut - 1_Lng))%U(:) =  &
                 Solution(i_reach)%UU(this%Model%DiscretizedReach(i_reach)%NCells_reach-1_Lng)%U(:)


          call MPI_ISEND(sent(1+2_Lng*(Counter_ReachCut-1_Lng):2+2_Lng*(Counter_ReachCut-1_Lng)), &
                     4, MPI_DOUBLE_PRECISION, this%Model%DiscretizedReach(i_reach)%CommRank-1,    &
                     tag_sent(Counter_ReachCut), MPI_COMM_WORLD, request_sent(Counter_ReachCut),  &
                     MPI_err)
          call MPI_IRECV(recv(1+2_Lng*(Counter_ReachCut-1_Lng):2+2_Lng*(Counter_ReachCut-1_Lng)), &
                      4, MPI_DOUBLE_PRECISION, this%Model%DiscretizedReach(i_reach)%CommRank-1,   &
                     tag_recv(Counter_ReachCut), MPI_COMM_WORLD, request_recv(Counter_ReachCut),  &
                     MPI_err)

      else if (this%Model%DiscretizedReach(i_reach)%Communication == 2_Tiny) then ! downstream half
                                                                  ! of this reach is on the rank
        ! We need to communicate with the rank that holds the upstream of this reach,
        ! the downstream of this reach is on this rank.
        ! The ghost cells are located at the downstream node, and no ghost cells on this rank for
        ! the upstream node.
        ! There is no ghost cell at the upstream, but, we need to communicate with the rank that
        ! holds the upstream of this reach to get the cells information.

        ! upstream cells
        ! get the solution from the rank that has the upstream
          if (this%Model%DiscretizedReach(i_reach)%BCNodeI /= -1_tiny) then
            ! check the correctness of the boundary condition- since the communication is 2, which
            ! means the downstream of this reach is on this rank, thus, this upstream node
            ! should not be on this rank. As a result, BCNodeII should be -1.
            write(*,*) " This number should be equal to -1. Double check the partitioner code."
            write(*,*) " Failed to proceed successfully. Check the solver subroutine!"
            stop
          end if

          Counter_ReachCut = Counter_ReachCut + 1_Lng

          ! communicate with the node that has the upstream part of this reach.
          ! Sending/Receiving cell info
          ! The upstream of this reach is on another rank
          sent(1+ 2_Lng*(Counter_ReachCut - 1_Lng))%U(:) = Solution(i_reach)%UU(1)%U(:)
          sent(2+ 2_Lng*(Counter_ReachCut - 1_Lng))%U(:) = Solution(i_reach)%UU(2)%U(:)

          call MPI_ISEND(sent(1+2_Lng*(Counter_ReachCut-1_Lng):2+2_Lng*(Counter_ReachCut-1_Lng)), &
                      4, MPI_DOUBLE_PRECISION, this%Model%DiscretizedReach(i_reach)%CommRank-1,   &
                      tag_sent(Counter_ReachCut), MPI_COMM_WORLD, request_sent(Counter_ReachCut), &
                      MPI_err)
          call MPI_IRECV(recv(1+2_Lng*(Counter_ReachCut-1_Lng):2+2_Lng*(Counter_ReachCut-1_Lng)), &
                      4, MPI_DOUBLE_PRECISION, this%Model%DiscretizedReach(i_reach)%CommRank-1,   &
                      tag_recv(Counter_ReachCut), MPI_COMM_WORLD, request_recv(Counter_ReachCut), &
                      MPI_err)

        ! downstream node
          if (this%Model%DiscretizedReach(i_reach)%BCNodeII == -1_tiny) then
            ! check the correctness of the boundary condition- since the communication is 2, which
            ! means that the downstream of this reach is on this rank, thus, this downstream node
            ! should be on this rank. As a result, BCNodeII should not be -1.
            write(*,*) " This number should not be equal to -1. Double check the partitioner code."
            write(*,*) " Failed to proceed successfully. Check the solver subroutine!"
            stop
          else if (this%Model%DiscretizedReach(i_reach)%BCNodeII == 0_tiny) then
            ! if the downstream node is a junction, not a BC. In this case, we need to decide about
            !  the node based on the junction modeling.
            call Junction_simulation_flow_combination_downstream(AnalysisInfo%Junction_Model,   &

                    this%Model%DiscretizedReach(i_reach)%ReachWidthCell,                 &
                    this%Model%DiscretizedReach(DownstreamReachNum)%ReachWidthCell,      &

                    Solution(i_reach)%UU(this%Model%DiscretizedReach(i_reach)%NCells_reach),    &
                    Solution(DownstreamReachNum)%UU(1),                                         &

                    Solution(i_reach)%UU(this%Model%DiscretizedReach(i_reach)%NCells_reach+1),  &
                    Solution(i_reach)%UU(this%Model%DiscretizedReach(i_reach)%NCells_reach+2))

          else if (this%Model%DiscretizedReach(i_reach)%BCNodeII == 1_tiny) then
            ! if the downstream node cannot be an inlet boundary condition
            ! <modify>
            write(*,*) " This number should not be equal to 1. Double check the partitioner code."
            write(*,*) " Failed to proceed successfully. Check the solver subroutine!"
            stop
          else if (this%Model%DiscretizedReach(i_reach)%BCNodeII == 2_tiny) then
            ! if the upstream node is a boundary condition/outlet
            call Impose_BC_1D_dw_sub( &
                   Solution(i_reach)%UU(this%Model%DiscretizedReach(i_reach)%NCells_reach)%U(2),  &
                   AnalysisInfo%h_dw,                                                             &
                   Solution(i_reach)%UU(this%Model%DiscretizedReach(i_reach)%NCells_reach+1_Lng), &
                   Solution(i_reach)%UU(this%Model%DiscretizedReach(i_reach)%NCells_reach+2_Lng))
          end if

      end if

  end do

! check on the number of communication with other ranks
  if ( this%Model%NCutsOnRanks /= Counter_ReachCut) then
    write(*,*)" Number of cuts on this rank does not match." , &
                                                          this%Model%NCutsOnRanks, Counter_ReachCut
    stop
  end if

! substituting the sent messages to the solution
Counter_ReachCut = 0_Lng
  do i_reach =1, this%Model%TotalNumOfReachesOnThisRank

    !write(*,       *) " *********** wait for reach: ", i_reach, this%ModelInfo%rank
    !write(FileInfo,*) " *********** wait for reach: ", i_reach, this%ModelInfo%rank

      if (this%Model%DiscretizedReach(i_reach)%Communication == 1_Tiny) then ! upstream half
                                                                   ! of this reach is on the rank
        Counter_ReachCut = Counter_ReachCut + 1_Lng
        call MPI_WAIT(request_sent(Counter_ReachCut), status , MPI_err)
        call MPI_WAIT(request_recv(Counter_ReachCut), status , MPI_err)

        ! Substituting the received data in the solution array
        Solution(i_reach)%UU(this%Model%DiscretizedReach(i_reach)%NCells_reach+1_Lng)%U(:) = &
                                                        recv(1+2_Lng*(Counter_ReachCut-1_Lng))%U(:)
        Solution(i_reach)%UU(this%Model%DiscretizedReach(i_reach)%NCells_reach+2_Lng)%U(:) = &
                                                        recv(2+2_Lng*(Counter_ReachCut-1_Lng))%U(:)

      else if (this%Model%DiscretizedReach(i_reach)%Communication == 2_Tiny) then ! downstream half
                                                                    ! of this reach is on the rank
        Counter_ReachCut = Counter_ReachCut + 1_Lng
        call MPI_WAIT(request_sent(Counter_ReachCut), status , MPI_err)
        call MPI_WAIT(request_recv(Counter_ReachCut), status , MPI_err)

        ! Substituting the received data in the solution array
        Solution(i_reach)%UU(0)%U(:)  = recv(1+ 2_Lng*(Counter_ReachCut - 1_Lng))%U(:)
        Solution(i_reach)%UU(-1)%U(:) = recv(2+ 2_Lng*(Counter_ReachCut - 1_Lng))%U(:)

      end if

  end do

Results%ModelInfo = this%ModelInfo

! preparing data for Paraview class
Paraview%RankNo    = this%ModelInfo%rank
Paraview%Size      = this%ModelInfo%size
Paraview%nReach    = this%Model%TotalNumOfReachesOnThisRank
Paraview%OutputDir = this%ModelInfo%ParaviewDir
Paraview%DT        = dt
Paraview%NoReachonRanks(:) = this%Model%NoReachonRanks(:)

! Creating the wrapper file for visualization in paraview
  if ( this%ModelInfo%rank == 0) then
    call Paraview%Wrapper_begin()
  end if

!call MPI_Barrier(MPI_COMM_WORLD, MPI_err) ! <delete> - for debugging purposes

write(*,       *) " -Time marching ..."
write(FileInfo,*) " -Time marching ..."

!!$OMP PARALLEL default(private) SHARED(UN,UU,S, dt, dx, dtdx)                                                                                                          firstprivate(this,SourceTerms,LimiterFunc,Jacobian_neighbor,Jacobian,alpha, alpha_neighbor, alpha_tilda, Wave, Wave_neighbor, Wave_tilda, F_L, F_H, Delta_U,TempSolution,i_steps, NSteps)
!$OMP PARALLEL default(none)  SHARED(UN,UU, dt, dx, dtdx) private(i_Cell,its,mts,height,velocity, Coefficient,height_interface, velocity_interface, speed, PrintResults) firstprivate(this,SourceTerms,LimiterFunc,Jacobian_neighbor, Jacobian,alpha, alpha_neighbor, alpha_tilda, Wave, Wave_neighbor, Wave_tilda, F_L, F_H, Delta_U,TempSolution, i_steps, NSteps, Results)

!$ ITS = OMP_GET_THREAD_NUM()
!$ MTS = OMP_GET_NUM_THREADS()

!$ write(*,       fmt="(' I am thread ',I4,' out of ',I4,' threads.')") ITS,MTS
!$ write(FileInfo,fmt="(' I am thread ',I4,' out of ',I4,' threads.')") ITS,MTS

  ! Time marching
  Time_Marching: do i_steps = 1_Lng, NSteps +1_Lng

    Counter_ReachCut = 0_Lng

      ! write down data for visualization
      if (mod(i_steps,AnalysisInfo%Plot_Inc)==1 .or. PrintResults) then
        !$ if (ITS==0) then

        Paraview%step = i_steps-1_Lng
          if ( this%ModelInfo%rank == 0) then
              call system_clock(TotalTime%endSys, TotalTime%clock_rate)
              write(*,fmt='("---Step:", I20," Elapsed time: ",F23.5," Simulation time: ",F23.5)') &
                    i_steps, i_steps*dt, &
                    real(TotalTime%endSys-TotalTime%startSys)/real(TotalTime%clock_rate)

            ! writing the name of local xdmf file in the wrapper file
            call Paraview%Wrapper_body()

          end if

        ! putting the results in the paraview array
          do i_reach = 1, this%Model%TotalNumOfReachesOnThisRank
            Paraview%ResultReach(i_reach)%U(:) = &
              Solution(i_reach)%UU(1:this%Model%DiscretizedReach(i_reach)%NCells_reach)
          end do

        ! writing the results in the hdf5 files
        call Paraview%Results()
      end if

      ! Solving the equation for each reach
      On_Reaches: do i_reach =1, this%Model%TotalNumOfReachesOnThisRank

          ! write down data for visualization
          if (mod(i_steps,AnalysisInfo%Plot_Inc)==1 .or. PrintResults) then

            ! writing the results for visualization in python - <modify>
            allocate(Results%U(-1:this%Model%DiscretizedReach(i_reach)%NCells_reach+2), &
                     stat=ERR_Alloc)
            if (ERR_Alloc /= 0) call error_in_allocation(ERR_Alloc)
            Results%NCells = this%Model%DiscretizedReach(i_reach)%NCells_reach
            Results%reach  = i_reach
            Results%step   = i_steps
            Results%U(:)   = &
                       Solution(i_reach)%UU(-1:this%Model%DiscretizedReach(i_reach)%NCells_reach+2)
            call Results%plot_results()
            deallocate(Results%U, stat=ERR_DeAlloc)
            if (ERR_DeAlloc /= 0) call error_in_deallocation(ERR_DeAlloc)
          end if

        UpstreamReachNumLeft   = this%Model%DiscretizedReach(i_reach)%UpstreamReaches(1,2)
        UpstreamReachNumRight  = this%Model%DiscretizedReach(i_reach)%UpstreamReaches(2,2)
        DownstreamReachNum     = this%Model%DiscretizedReach(i_reach)%DownstreamReaches(1,2)

        dx     = this%Model%DiscretizedReach(i_reach)%LengthCell(1)
        dtdx = dt / dx

          !$OMP DO
          ! Loop over cells except the boundary cells
          On_Cells: do i_Cell = 1_Lng, this%Model%DiscretizedReach(i_reach)%NCells_reach

            ! Initialize fluxes
            F_L%U(:) = 0.0_Dbl ! upwind flux (not exactly, see notes)
            F_H%U(:) = 0.0_Dbl ! lax-Wendroff flux (not exactly, see notes)

            SourceTerms%Source_1%U(:)  = 0.0_Dbl
            SourceTerms%Source_2%U(:)  = 0.0_Dbl

            ! Solution at this cell
            height   = Solution(i_reach)%UU(i_Cell)%U(1)
            velocity = Solution(i_reach)%UU(i_Cell)%U(2)/height

            ! Find the B matrix for this cell
            SourceTerms%S_f = (this%Model%DiscretizedReach(i_reach)%ReachManning**2.0) * velocity &
                              * dabs(velocity)/(height**(4.0_Dbl/3.0_Dbl))

            SourceTerms%B(1,1) = 0.0_Dbl
            SourceTerms%B(2,1) =  &
                     + Gravity * (this%Model%DiscretizedReach(i_reach)%CellSlope(i_Cell) + &
                                                               (7.0_Dbl/3.0_Dbl) * SourceTerms%S_f)

            SourceTerms%B(1,2) = 0.0_Dbl
            SourceTerms%B(2,2) = &
                        -(2.0_Dbl*this%Model%DiscretizedReach(i_reach)%ReachManning**2.0) &
                                                        *dabs(velocity)/(height**(4.0_Dbl/3.0_Dbl))

                        !-(2.0_Dbl*Gravity*  SourceTerms%S_f) /velocity
                        ! do not use the above equation bcs of division by zero in case v=0

            ! Find the BI
            SourceTerms%BI(:,:) = SourceTerms%Identity - 0.5_Dbl * dt * SourceTerms%B(:,:)

            call Inverse(SourceTerms%BI(:,:), SourceTerms%BI(:,:)) ! the inverse

            ! Source terms at the current cell/time
            SourceTerms%S%U(1) = 0.0_Dbl
            SourceTerms%S%U(2) =&
                       -Gravity*height*(this%Model%DiscretizedReach(i_reach)%CellSlope(i_Cell)- &
                                                                                   SourceTerms%S_f)

            ! The first contribution of the source term in the solution
            SourceTerms%Source_1%U(:) = &
                         dt * (SourceTerms%S%U(:) &
                         - 0.5_Dbl * matmul(SourceTerms%B(:,:), Solution(i_reach)%UU(i_Cell)%U(:)))


              On_Interface:  do i_Interface = 1, 2
                !first one is on i-1/2, the second one is on i+1/2

                ! Compute the jump (U_i- U_i-1)
                Delta_U%U(:) =Solution(i_reach)%UU(i_Cell+(i_Interface-1_Lng))%U(:)- &
                              Solution(i_reach)%UU(i_Cell+(i_Interface-2_Lng))%U(:)

                ! Computing the Jacobian and all other items at the upstream
                Jacobian%U_up%U(:) = Solution(i_reach)%UU(i_Cell+(i_Interface-2_Lng))%U(:)
                Jacobian%U_dw%U(:) = Solution(i_reach)%UU(i_Cell+(i_Interface-1_Lng))%U(:)

                call Jacobian%Jacobian()

                ! Compute alpha(= RI*(U_i - U_(i-1))
                alpha%U(:) = matmul(Jacobian%L, Delta_U%U(:))

                Coefficient = (-1.0_Dbl) ** i_Interface

                height_interface = 0.5_Dbl * (Jacobian%U_up%U(1) +Jacobian%U_dw%U(1) )

                velocity_interface = 0.5_Dbl*(Jacobian%U_up%U(2)/Jacobian%U_up%U(1)+  &
                                                            Jacobian%U_dw%U(2)/Jacobian%U_dw%U(1))

                SourceTerms%S_f_interface = &
                             this%Model%DiscretizedReach(i_reach)%ReachManning*velocity_interface &
                             *dabs(velocity_interface) /( height_interface**(4.0_Dbl/3.0_Dbl))

                SourceTerms%S_interface%U(1) = 0.0_Dbl
                SourceTerms%S_interface%U(2) =-Gravity*height_interface &
                *(this%Model%DiscretizedReach(i_reach)%InterfaceSlope(i_Cell+i_Interface-1_Tiny)- &
                  SourceTerms%S_f_interface)

                SourceTerms%Source_2%U(:) = SourceTerms%Source_2%U(:) + 0.5_Dbl * (dt**2) / dx &
                                 *( Coefficient * matmul(Jacobian%A, SourceTerms%S_interface%U(:)))

                if ( alpha%U(1) ==0.0_Dbl .and. alpha%U(2) ==0.0_Dbl) cycle

                  On_Eigenvalues: do i_eigen = 1_Tiny, 2_Tiny

                    Wave%U(:) = alpha%U(i_eigen) * Jacobian%R(:,i_eigen)

                      ! We use positive eigenvalues on the upstream interface
                      if (i_Interface == 1_Tiny) then
                        speed = Jacobian%Lambda_plus%U(i_eigen)
                        !will take care of the sign of flux in the high-resolution part
                        Coefficient = -1.0_Dbl
                      ! We use negative eigenvalues on the downstream interface
                      else if (i_Interface == 2_Tiny) then
                        speed = Jacobian%Lambda_minus%U(i_eigen)
                        ! will take care of the sign of flux in high-resolution part
                        Coefficient = +1.0_Dbl
                      end if

                    ! The upwind part
                    F_L%U(:) = F_L%U(:) + speed * Wave%U(:)

                      ! This if condition computes the W_(I-1/2)
                      if  (Jacobian%Lambda%U(i_eigen)  > 0.0_Dbl ) then

                        ! Compute the jump (U_i- U_i-1)
                        Delta_U%U(:) = Solution(i_reach)%UU(i_Cell+i_Interface-2_Tiny)%U(:) &
                                      -Solution(i_reach)%UU(i_Cell+i_Interface-3_Tiny )%U(:)

                        ! Computing the Jacobian and all other items at the upstream
                        Jacobian_neighbor%U_up%U(:) = &
                                               Solution(i_reach)%UU(i_Cell+i_Interface-3_Tiny)%U(:)

                        Jacobian_neighbor%U_dw%U(:) = &
                                               Solution(i_reach)%UU(i_Cell+i_Interface-2_Tiny)%U(:)

                        call Jacobian_neighbor%Jacobian()

                        ! Compute alpha(= RI*(U_i - U_(i-1))
                        alpha_neighbor%U(:) = matmul(Jacobian_neighbor%L(:,:), Delta_U%U(:))
                        Wave_neighbor%U(:)  = alpha_neighbor%U(i_eigen)* &
                                              Jacobian_neighbor%R(:, i_eigen)

                      else if  (Jacobian%Lambda%U(i_eigen) < 0.0_Dbl) then

                        ! Compute the jump (U_i- U_i-1)
                        Delta_U%U(:) = Solution(i_reach)%UU(i_Cell+i_Interface)%U(:)- &
                                               Solution(i_reach)%UU(i_Cell+i_Interface-1_Tiny)%U(:)

                        ! Computing the Jacobian and all other items at the upstream
                        Jacobian_neighbor%U_up%U(:) = &
                                               Solution(i_reach)%UU(i_Cell+i_Interface-1_Tiny)%U(:)
                        Jacobian_neighbor%U_dw%U(:) = &
                                               Solution(i_reach)%UU(i_Cell+i_Interface       )%U(:)

                        call Jacobian_neighbor%Jacobian()

                        ! Compute alpha(= RI*(U_i - U_(i-1))
                        alpha_neighbor%U(:) = matmul(Jacobian_neighbor%L(:,:), Delta_U%U(:))
                        Wave_neighbor%U(:)  = alpha_neighbor%U(i_eigen)* &
                                                                     Jacobian_neighbor%R(:,i_eigen)

                      else
                        write(*,*)" Something is wrong. Check the limiter subroutine."
                        write(*,fmt='("The eigenvalue is wrong (most probably NaN): ",5I5,F20.5)')&
                        i_eigen,i_reach,i_Cell,i_steps,this%ModelInfo%rank,Jacobian%Lambda%U(i_eigen)
                        stop
                      end if

                      if ( dot_product(Wave%U(:), Wave%U(:) ) /= 0.0_Dbl ) then
                        LimiterFunc%theta = (dot_product(Wave_neighbor%U(:),Wave%U(:))) &
                                            /(dot_product(Wave%U(:), Wave%U(:) )  )
                      else
                        LimiterFunc%theta = 0.0_Dbl
                      end if

                    ! The limiter function
                    call LimiterFunc%LimiterValue()

                    !LimiterFunc%phi =0.0
                    alpha_tilda%U(:) =  LimiterFunc%phi * alpha%U(:)

                    !theta (2*(i_Cell-1)+i_Interface)%U(i_eigen) = LimiterFunc%theta
                    !phi   (2*(i_Cell-1)+i_Interface)%U(i_eigen) = LimiterFunc%phi

                    Wave_tilda%U(:) = alpha_tilda%U(i_eigen) * Jacobian%R(:,i_eigen)

                    ! The high-resolution (Lax-Wendroff) part
                    F_H%U(:) = F_H%U(:) + Coefficient*0.5_Dbl * dabs(Jacobian%Lambda%U(i_eigen) ) &
                               * (1.0_Dbl-dtdx*dabs(Jacobian%Lambda%U(i_eigen)))*Wave_tilda%U(:)

                  end do On_Eigenvalues
              end do On_Interface

            ! Final update the results
            TempSolution%U(:) = Solution(i_reach)%UU(i_cell)%U(:) - dtdx*F_L%U(:)-dtdx * F_H%U(:) &
                                + SourceTerms%Source_1%U(:) - SourceTerms%Source_2%U(:)

            ! do not <delete>, useful for debugging
            !write(*,fmt='(9(2F10.2,6x))')Solution(i_reach)%UU(i_cell)%U(:), &
            !                            dtdx*F_L%U(:), &
            !                           dtdx * F_H%U(:), &
            !                            SourceTerms%Source_1%U(:), &
            !                            SourceTerms%Source_2%U(:), &
            !                            SourceTerms%S%U(:), &
            !                            SourceTerms%B(:,:), &
            !                            Solution(i_reach)%UU(i_Cell)%U(:)

            Solution(i_reach)%UN(i_cell)%U(:) = matmul(SourceTerms%BI(:,:), TempSolution%U(:))

          end do On_Cells
          !$OMP END DO

          !!$OMP END PARALLEL DO
          !$OMP DO
          ! Loop over cells except the boundary cells
          do i_Cell = 1_Lng, this%Model%DiscretizedReach(i_reach)%NCells_reach
            Solution(i_reach)%UU(i_Cell) = Solution(i_reach)%UN(i_Cell)
          end do
          !$OMP END DO

          !$OMP single
          ! apply boundary condition
          ! The following section is basically, identical to the section above. Before the loops.
          ! working on the ghost cells, or on the junction nodes, for each reach
          if (this%Model%DiscretizedReach(i_reach)%Communication == -1_Tiny) then
            ! no communication with other ranks, the entire reach is on this rank.
            ! There are 2 ghost cells at each ends of this reach, where the nodes are located.


              ! upstream node --
              if (this%Model%DiscretizedReach(i_reach)%BCNodeI == 0_tiny) then
                ! if the upstream node is a junction, no BC. In this case, we need to decide about
                !  the node based on the junction modeling.
                call Junction_simulation_flow_combination_upstream(AnalysisInfo%Junction_Model,   &

                this%Model%DiscretizedReach(UpstreamReachNumLeft)%ReachWidthCell,  &
                this%Model%DiscretizedReach(UpstreamReachNumRight)%ReachWidthCell, &
                this%Model%DiscretizedReach(i_reach)%ReachWidthCell,    &

                Solution(UpstreamReachNumLeft)%UU(this%Model%DiscretizedReach(UpstreamReachNumLeft)%NCells_reach),     &
                Solution(UpstreamReachNumRight)%UU(this%Model%DiscretizedReach(UpstreamReachNumRight)%NCells_reach),   &
                Solution(i_reach)%UU(1),                                &

                Solution(i_reach)%UU(0),                                &
                Solution(i_reach)%UU(-1))

              else if (this%Model%DiscretizedReach(i_reach)%BCNodeI == 1_tiny) then
                ! if the upstream node is a boundary condition/inlet
                call Impose_BC_1D_up_sub(Solution(i_reach)%UU(1)%U(1),                         &
                                         this%Model%DiscretizedReach(i_reach)%Q_Up,            &
                                         this%Model%DiscretizedReach(i_reach)%ReachWidthCell,  &
                                         Solution(i_reach)%UU(-1_Lng), Solution(i_reach)%UU(0_Lng))
              end if

              ! downstream node
              if (this%Model%DiscretizedReach(i_reach)%BCNodeII == 0_tiny) then
                ! if the downstream node is a junction, no BC. In this case, we need to decide
                ! about the node based on the junction modeling.
                call Junction_simulation_flow_combination_downstream(AnalysisInfo%Junction_Model, &

                        this%Model%DiscretizedReach(i_reach)%ReachWidthCell,                 &
                        this%Model%DiscretizedReach(DownstreamReachNum)%ReachWidthCell,      &

                        Solution(i_reach)%UU(this%Model%DiscretizedReach(i_reach)%NCells_reach),  &
                        Solution(DownstreamReachNum)%UU(1),                                       &

                        Solution(i_reach)%UU(this%Model%DiscretizedReach(i_reach)%NCells_reach+1),&
                        Solution(i_reach)%UU(this%Model%DiscretizedReach(i_reach)%NCells_reach+2))

              else if (this%Model%DiscretizedReach(i_reach)%BCNodeII == 2_tiny) then
                ! if the upstream node is a boundary condition/outlet
                call Impose_BC_1D_dw_sub( &
                   Solution(i_reach)%UU(this%Model%DiscretizedReach(i_reach)%NCells_reach)%U(2),  &
                   AnalysisInfo%h_dw,                                                             &
                   Solution(i_reach)%UU(this%Model%DiscretizedReach(i_reach)%NCells_reach+1_Lng), &
                   Solution(i_reach)%UU(this%Model%DiscretizedReach(i_reach)%NCells_reach+2_Lng))
              end if



          else if (this%Model%DiscretizedReach(i_reach)%Communication == 1_Tiny) then!upstream half
                                                                     ! of this reach is on the rank
            ! We need to communicate with the rank that holds the downstream of this reach,
            ! the upstream of this reach is on this rank.
            ! The ghost cells at the upstream are at the junction, which is located on this rank.
            ! There is no ghost cell at the downstream, but, we need to communicate with the rank
            ! that holds the downstream of this reach to get the cells information at the bottom.

            ! upstream node
              if (this%Model%DiscretizedReach(i_reach)%BCNodeI == 0_tiny) then
                ! if the upstream node is a junction, no BC. In this case, we need to decide about
                ! the node based on the junction modeling.
                call Junction_simulation_flow_combination_upstream(AnalysisInfo%Junction_Model,   &

                this%Model%DiscretizedReach(UpstreamReachNumLeft)%ReachWidthCell,  &
                this%Model%DiscretizedReach(UpstreamReachNumRight)%ReachWidthCell, &
                this%Model%DiscretizedReach(i_reach)%ReachWidthCell,    &

                Solution(UpstreamReachNumLeft)%UU(this%Model%DiscretizedReach(UpstreamReachNumLeft)%NCells_reach),     &
                Solution(UpstreamReachNumRight)%UU(this%Model%DiscretizedReach(UpstreamReachNumRight)%NCells_reach),   &
                Solution(i_reach)%UU(1),                                &

                Solution(i_reach)%UU(0),                                &
                Solution(i_reach)%UU(-1))

              else if (this%Model%DiscretizedReach(i_reach)%BCNodeI == 1_tiny) then
                ! if the upstream node is a boundary condition/inlet
                call Impose_BC_1D_up_sub(Solution(i_reach)%UU(1)%U(1),                         &
                                         this%Model%DiscretizedReach(i_reach)%Q_Up,            &
                                         this%Model%DiscretizedReach(i_reach)%ReachWidthCell,  &
                                         Solution(i_reach)%UU(-1_Lng), Solution(i_reach)%UU(0_Lng))
              end if



            ! downstream cells
              ! get the solution from the rank that has the downstream

              Counter_ReachCut = Counter_ReachCut + 1_Lng

              ! communicate with the node that has the downstream part of this reach.
              ! Sending/Receiving cell info
              ! The downstream of this reach is on another rank
              sent(1+ 2_Lng*(Counter_ReachCut - 1_Lng))%U(:) =  &
                       Solution(i_reach)%UU(this%Model%DiscretizedReach(i_reach)%NCells_reach)%U(:)
              sent(2+ 2_Lng*(Counter_ReachCut - 1_Lng))%U(:) =  &
                 Solution(i_reach)%UU(this%Model%DiscretizedReach(i_reach)%NCells_reach-1_Lng)%U(:)

              call MPI_ISEND(  &
                      sent(1+2_Lng*(Counter_ReachCut-1_Lng):2+2_Lng*(Counter_ReachCut-1_Lng)),   &
                      4, MPI_DOUBLE_PRECISION, this%Model%DiscretizedReach(i_reach)%CommRank-1,  &
                      tag_sent(Counter_ReachCut), MPI_COMM_WORLD, request_sent(Counter_ReachCut),&
                      MPI_err)
              call MPI_IRECV( &
                     recv(1+2_Lng*(Counter_ReachCut-1_Lng):2+2_Lng*(Counter_ReachCut-1_Lng)),   &
                     4,MPI_DOUBLE_PRECISION, this%Model%DiscretizedReach(i_reach)%CommRank-1,   &
                     tag_recv(Counter_ReachCut), MPI_COMM_WORLD, request_recv(Counter_ReachCut),&
                     MPI_err)


          else if (this%Model%DiscretizedReach(i_reach)%Communication == 2_Tiny) then!dwstream half
                                                                     ! of this reach is on the rank
            ! We need to communicate with the rank that holds the upstream of this reach,
            ! the downstream of this reach is on this rank.
            ! The ghost cells are located at the downstream node, and no ghost cells on this rank
            ! for the upstream node.
            ! There is no ghost cell at the upstream, but, we need to communicate with the rank
            ! that holds the upstream of this reach to get the cells information.

            ! upstream cells
            ! get the solution from the rank that has the upstream

              Counter_ReachCut = Counter_ReachCut + 1_Lng

              ! communicate with the node that has the upstream part of this reach.
              ! Sending/Receiving cell info
              ! The upstream of this reach is on another rank
              sent(1+ 2_Lng*(Counter_ReachCut - 1_Lng))%U(:) = Solution(i_reach)%UU(1)%U(:)
              sent(2+ 2_Lng*(Counter_ReachCut - 1_Lng))%U(:) = Solution(i_reach)%UU(2)%U(:)
              call MPI_ISEND(  &
                      sent(1+2_Lng*(Counter_ReachCut-1_Lng):2+2_Lng*(Counter_ReachCut-1_Lng)),    &
                      4, MPI_DOUBLE_PRECISION, this%Model%DiscretizedReach(i_reach)%CommRank-1,   &
                      tag_sent(Counter_ReachCut), MPI_COMM_WORLD, request_sent(Counter_ReachCut), &
                      MPI_err)
              call MPI_IRECV( &
                      recv(1+2_Lng*(Counter_ReachCut-1_Lng):2+2_Lng*(Counter_ReachCut-1_Lng)),    &
                      4, MPI_DOUBLE_PRECISION, this%Model%DiscretizedReach(i_reach)%CommRank-1,   &
                      tag_recv(Counter_ReachCut), MPI_COMM_WORLD, request_recv(Counter_ReachCut), &
                      MPI_err)

            ! downstream node
              if (this%Model%DiscretizedReach(i_reach)%BCNodeII == 0_tiny) then
                ! if the downstream node is a junction, no BC. In this case, we need to decide
                ! about the node based on the junction modeling.
                call Junction_simulation_flow_combination_downstream(AnalysisInfo%Junction_Model, &

                        this%Model%DiscretizedReach(i_reach)%ReachWidthCell,                 &
                        this%Model%DiscretizedReach(DownstreamReachNum)%ReachWidthCell,      &

                        Solution(i_reach)%UU(this%Model%DiscretizedReach(i_reach)%NCells_reach),  &
                        Solution(DownstreamReachNum)%UU(1),                                       &

                        Solution(i_reach)%UU(this%Model%DiscretizedReach(i_reach)%NCells_reach+1),&
                        Solution(i_reach)%UU(this%Model%DiscretizedReach(i_reach)%NCells_reach+2))
              else if (this%Model%DiscretizedReach(i_reach)%BCNodeII == 2_tiny) then
                ! if the upstream node is a boundary condition/outlet
                call Impose_BC_1D_dw_sub(  &
                   Solution(i_reach)%UU(this%Model%DiscretizedReach(i_reach)%NCells_reach)%U(2),  &
                   AnalysisInfo%h_dw,                        &
                   Solution(i_reach)%UU(this%Model%DiscretizedReach(i_reach)%NCells_reach+1_Lng), &
                   Solution(i_reach)%UU(this%Model%DiscretizedReach(i_reach)%NCells_reach+2_Lng))
              end if

          end if

      !$OMP end single
      end do On_Reaches

    ! substituting the sent messages to the solution
    Counter_ReachCut = 0_Lng
      do i_reach =1, this%Model%TotalNumOfReachesOnThisRank

          if (this%Model%DiscretizedReach(i_reach)%Communication == 1_Tiny) then ! upstream half
                                                                    ! of this reach is on the rank

            Counter_ReachCut = Counter_ReachCut + 1_Lng

            call MPI_WAIT(request_recv(Counter_ReachCut), status , MPI_err)
            call MPI_WAIT(request_sent(Counter_ReachCut), status , MPI_err)

            ! Substituting the received data in the solution array
            Solution(i_reach)%UU(this%Model%DiscretizedReach(i_reach)%NCells_reach+1_Lng)%U(:) = &
                                                        recv(1+2_Lng*(Counter_ReachCut-1_Lng))%U(:)
            Solution(i_reach)%UU(this%Model%DiscretizedReach(i_reach)%NCells_reach+2_Lng)%U(:) = &
                                                        recv(2+2_Lng*(Counter_ReachCut-1_Lng))%U(:)

          else if (this%Model%DiscretizedReach(i_reach)%Communication == 2_Tiny) then!dwstream half
                                                                    ! of this reach is on the rank

            Counter_ReachCut = Counter_ReachCut + 1_Lng

            call MPI_WAIT(request_recv(Counter_ReachCut), status , MPI_err)
            call MPI_WAIT(request_sent(Counter_ReachCut), status , MPI_err)

            ! Substituting the received data in the solution array
            Solution(i_reach)%UU(0 )%U(:) = recv(1+ 2_Lng*(Counter_ReachCut - 1_Lng))%U(:)
            Solution(i_reach)%UU(-1)%U(:) = recv(2+ 2_Lng*(Counter_ReachCut - 1_Lng))%U(:)

          end if

      end do

  end do Time_Marching

  !$OMP END PARALLEL

! Closing the wrapper file for visualization in paraview
  if ( this%ModelInfo%rank == 0) then
    call Paraview%Wrapper_close()
  end if

write(*,       *) " end subroutine < solve_the_network_sub >"
write(FileInfo,*) " end subroutine < solve_the_network_sub >"
return

end subroutine solve_the_network_sub


!##################################################################################################
! Purpose: This subroutine imposes the boundary conditions on a 1D domain.
!
! Developed by: Babak Poursartip
!
! Supervised by: Clint Dawson
! The Institute for Computational Engineering and Sciences (ICES)
! The University of Texas at Austin
!
! ================================ V E R S I O N ==================================================
! V0.00: 03/15/2018 - File initiated.
! V0.01: 03/15/2018 - Initiated: Compiled without error for the first time.
!
! File version $Id $
!
! Last update: 03/15/2018
!
! ================================ L O C A L   V A R I A B L E S ==================================
! (Refer to the main code to see the list of imported variables)
!  . . . . . . . . . . . . . . . . Variables . . . . . . . . . . . . . . . . . . . . . . . . . . .
!
!##################################################################################################

!pure subroutine Impose_BC_1D_up_sub(h_upstream, Q_Up, Width, UU_N1,UU_0)    ! <modify>
subroutine Impose_BC_1D_up_sub(h_upstream, Q_Up, Width, UU_N1,UU_0)

! Libraries =======================================================================================

! User defined modules ============================================================================

implicit none

! Global variables ================================================================================

real(kind=DBL), intent(in)    :: h_upstream, Q_Up, Width

! Local variables =================================================================================
type(vector), intent(out)   :: UU_N1,UU_0

! code ============================================================================================
!write(*,       *) " subroutine < Impose_BC_1D_up_sub >: "
!write(FileInfo,*) " subroutine < Impose_BC_1D_up_sub >: "

! Boundary conditions on the height
UU_N1%U(1) = h_upstream ! h at the upstream
UU_0%U(1)  = h_upstream ! h at the upstream

! Boundary conditions on the discharge
UU_N1%U(2) = Q_Up / Width ! uh at the upstream
UU_0%U(2)  = Q_Up / Width  ! uh at the upstream

!write(*,       *) " end subroutine < Impose_BC_1D_up_sub >"
!write(FileInfo,*) " end subroutine < Impose_BC_1D_up_sub >"
return
end subroutine Impose_BC_1D_up_sub


pure subroutine Impose_BC_1D_dw_sub(Q_dw, h_dw, UU_NCp1,UU_NCp2)

! Libraries =======================================================================================

! User defined modules ============================================================================

implicit none

! Global variables ================================================================================
real(kind=DBL), intent(in) :: Q_dw   ! this parameter is in fact "uh"
real(kind=DBL), intent(in) :: h_dw

! Local variables =================================================================================
type(vector), intent(out)   :: UU_NCp1,UU_NCp2

! code ============================================================================================
!write(*,       *) " subroutine < Impose_BC_1D_dw_sub >: "
!write(FileInfo,*) " subroutine < Impose_BC_1D_dw_sub >: "

! Boundary conditions on the height
UU_NCp1%U(1) = h_dw ! h at the downstream
UU_NCp2%U(1) = h_dw ! h at the downstream

! Boundary conditions on the discharge
UU_NCp1%U(2) = Q_dw ! discharge at the upstream at the downstream
UU_NCp2%U(2) = Q_dw !  at the downstream

!write(*,       *) " end subroutine < Impose_BC_1D_dw_sub >"
!write(FileInfo,*) " end subroutine < Impose_BC_1D_dw_sub >"
return
end subroutine Impose_BC_1D_dw_sub

!##################################################################################################
! Purpose: This subroutine contains various limiters functions.
!
! Developed by: Babak Poursartip
! Supervised by: Clint Dawson
!
! The Institute for Computational Engineering and Sciences (ICES)
! The University of Texas at Austin
!
! ================================ V E R S I O N ==================================================
! V0.00: 03/15/2018 - Subroutine initiated.
! V0.01: 03/15/2018 - Initiated: Compiled without error for the first time.
!
! File version $Id $
!
! Last update: 03/15/2018
!
! ================================ L O C A L   V A R I A B L E S ==================================
! (Refer to the main code to see the list of imported variables)
!  . . . . . . . . . . . . . . . . Variables . . . . . . . . . . . . . . . . . . . . . . . . . . .
!
!##################################################################################################

subroutine Limiters_sub(this)


! Libraries =======================================================================================

! User defined modules ============================================================================


implicit none

! Global variables ================================================================================
! - types -----------------------------------------------------------------------------------------
class(LimiterFunc_tp), intent(inout) :: this

! Local variables =================================================================================

! code ============================================================================================
!write(*,       *) " subroutine < Limiters_sub >: "
!write(FileInfo,*) " subroutine < Limiters_sub >: "

  select case (this%limiter_Type)

    case(-1)  ! Lax-Wendroff
      this%phi = 1.0_Dbl
    case(0)  ! upwind
      this%phi = 0.0_Dbl
    case(1)  ! minmod: most diffusive
      this%phi = dmax1(  0.0_Dbl, dmin1( 1.0_Dbl, this%theta )  )
    case(2)  !superbee
      this%phi = dmax1( 0.0_Dbl, dmin1( 1.0_Dbl, 2.0_Dbl*this%theta ), dmin1( 2.0_Dbl, this%theta))
    case(3)  ! MC (Woodward)
      this%phi = dmax1( 0.0_Dbl,dmin1( (1.0_Dbl + this%theta)/2.0_Dbl,2.0_Dbl,2.0_Dbl*this%theta))
    case(4)  ! van Leer
      this%phi = ( this%theta +  dabs(this%theta) ) / ( 1.0_Dbl + dabs(this%theta) )
    case default
      write(*,*)" The limiter type does exist, please select a limiter from the list &
                  and modify the input file."
      write(FileInfo,*)" The limiter type does exist, please select a limiter from the list &
                         and modify the input file."
      write(*,*)
      write(FileInfo,*)
      write(*,*)" Simulation terminated with error."
      write(*, Fmt_End);  read(*,*);   stop;

  end select

!write(*,       *) " end subroutine < Limiters_sub >"
!write(FileInfo,*) " end subroutine < Limiters_sub >"
return
end subroutine Limiters_sub


!##################################################################################################
! Purpose: This subroutine computes the Jacobian matrix, Jacobian plus, and
!          Jacobian minus at each cell.
!
! Developed by: Babak Poursartip
! Supervised by: Clint Dawson
!
! The Institute for Computational Engineering and Sciences (ICES)
! The University of Texas at Austin
!
! ================================ V E R S I O N ==================================================
! V0.00: 03/15/2018 - Subroutine initiated.
! V0.01: 03/15/2018 - Initiated: Compiled without error for the first time.
! V1.00: 05/09/2018 - Modifying the performance
!
! File version $Id $
!
! Last update: 05/09/2018
!
! ================================ L O C A L   V A R I A B L E S ==================================
! (Refer to the main code to see the list of imported variables)
!  . . . . . . . . . . . . . . . . Variables . . . . . . . . . . . . . . . . . . . . . . . . . . .
!
!##################################################################################################

subroutine Jacobian_sub(this)


! Libraries =======================================================================================

! User defined modules ============================================================================


implicit none

! Global variables ================================================================================
! - types -----------------------------------------------------------------------------------------
class(Jacobian_tp), intent(inout) :: this

! Local variables =================================================================================

real(kind=Dbl) :: h_dw  ! the height at the downstream grid
real(kind=Dbl) :: u_dw  ! the velocity at the downstream grid

real(kind=Dbl) :: h_up  ! the height at the upstream grid
real(kind=Dbl) :: u_up  ! the velocity at the upstream grid

real(kind=Dbl) :: u_ave   ! the average velocity at the interface
real(kind=Dbl) :: h_ave  ! the average height at the interface

real(kind=Dbl) :: c  ! wave speed

real(kind=Dbl), dimension(2,2) :: A_up  ! the average discharge at the interface
real(kind=Dbl), dimension(2,2) :: A_dw  ! the average discharge at the interface

! code ============================================================================================
!write(*,       *) " subroutine <Jacobian_sub>: "
!write(FileInfo,*) " subroutine <Jacobian_sub>: "

  ! Find the average solution at the interface and then compute the Jacobian
  if (this%option == 1 ) then

    h_dw = this%U_dw%U(1)
    u_dw = this%U_dw%U(2)/ h_dw !this%U_dw%U(1)

    h_up = this%U_up%U(1)
    u_up = this%U_up%U(2)/h_up !this%U_up%U(1)

    h_ave = 0.5_Dbl*(h_up+h_dw)   ! <modify> for unstructured discretization
    u_ave = 0.5_Dbl*(u_up+u_dw)   ! <modify> for unstructured discretization

    ! Computing the Jacobian - A
    this%A(1,1) = 0.0_Dbl
    this%A(1,2) = 1.0_Dbl
    this%A(2,1) = Gravity * h_ave - u_ave*u_ave
    this%A(2,2) = 2.0_Dbl * u_ave

    ! Computing the eigenvalues
    c = dsqrt (Gravity * h_ave)   ! wave speed

    ! do not <delete>, useful for debugging purposes
    !write(*, fmt='("h uh: ", 4F10.4)') h_up, h_dw, u_up, u_dw ! do not <delete>
    !print*,"inside the jacobian: ", h_ave, u_ave !u_ave, c  ! do not <delete>


    this%Lambda%U(1) = u_ave - c
    this%Lambda%U(2) = u_ave + c

    this%Lambda_plus%U(1) =  dmax1(this%Lambda%U(1), 0.0_Dbl)
    this%Lambda_plus%U(2) =  dmax1(this%Lambda%U(2), 0.0_Dbl)

    this%Lambda_minus%U(1) =  dmin1(this%Lambda%U(1), 0.0_Dbl)
    this%Lambda_minus%U(2) =  dmin1(this%Lambda%U(2), 0.0_Dbl)

    ! Computing the eigenvectors
    this%R(1,1) = 1.0_Dbl
    this%R(2,1) = this%Lambda%U(1)

    this%R(1,2) = 1.0_Dbl
    this%R(2,2) = this%Lambda%U(2)

    ! Computing the eigenvectors inverse
    this%L(1,1) = this%Lambda%U(2)
    this%L(1,2) = -1.0_Dbl

    this%L(2,1) = -this%Lambda%U(1)
    this%L(2,2) = 1.0_Dbl

    this%L(:,:)  =  this%L(:,:) /(2.0_Dbl * c)

    ! Fill eigenvalue matrix
    !this%Gam(1,1) = this%Lambda%U(1)
    !this%Gam(1,2) = 0.0_Dbl
    !this%Gam(2,1) = 0.0_Dbl
    !this%Gam(2,2) = this%Lambda%U(2)

    ! Fill Gamma plus
    !this%Gam_plus(1,1) = dmax1(this%Lambda%U(1), 0.0_Dbl)
    !this%Gam_plus(1,2) = 0.0_Dbl
    !this%Gam_plus(2,1) = 0.0_Dbl
    !this%Gam_plus(2,2) = dmax1(this%Lambda%U(2), 0.0_Dbl)

    ! Fill Gamma minus
    !this%Gam_minus(1,1) = dmin1(this%Lambda%U(1), 0.0_Dbl)
    !this%Gam_minus(1,2) = 0.0_Dbl
    !this%Gam_minus(2,1) = 0.0_Dbl
    !this%Gam_minus(2,2) = dmin1(this%Lambda%U(2), 0.0_Dbl)

    ! Compute A plus
    !this%A_plus  = matmul(matmul(this%R, this%Gam_plus), this%L)

    ! Compute A minus
    !this%A_minus = matmul(matmul(this%R, this%Gam_minus), this%L)

    ! Compute A abs
    !this%A_abs = this%A_plus - this%A_minus

  ! Find the Jacobian at each grid and average the Jacobian to find the Jacobian at the interface
  else if (this%option == 2 ) then

    ! Computing the Jacobian at the upstream - A
    h_up = this%U_up%U(1)
    u_up = this%U_up%U(2) / this%U_up%U(1)

    A_up(1,1) = 0.0_Dbl
    A_up(1,2) = 1.0_Dbl
    A_up(2,1) = Gravity * h_up - u_up**2
    A_up(2,2) = 2.0_Dbl * u_up


    ! Computing the Jacobian at the upstream - A
    h_dw = this%U_dw%U(1)
    u_dw = this%U_dw%U(2) / this%U_dw%U(1)

    A_dw(1,1) = 0.0_Dbl
    A_dw(1,2) = 1.0_Dbl
    A_dw(2,1) = Gravity * h_dw - u_dw**2
    A_dw(2,2) = 2.0_Dbl * u_dw

    ! average Jacobian
    this%A(:,:) = 0.5_Dbl * ( A_up(:,:) + A_dw(:,:) )

    ! Computing the eigenvalues
    call Eigenvalues_sub(this%A, this%Lambda%U(1), this%Lambda%U(2))

    ! Computing the eigenvectors
    this%R(1,1) = 1.0_Dbl
    this%R(2,1) = this%Lambda%U(1)

    this%R(1,2) = 1.0_Dbl
    this%R(2,2) = this%Lambda%U(2)

    ! Computing the eigenvectors inverse
    this%L(1,1) = this%Lambda%U(2)
    this%L(1,2) = -1.0_Dbl

    this%L(2,1) = -this%Lambda%U(1)
    this%L(2,2) = 1.0_Dbl

    c = this%Lambda%U(2) - this%Lambda%U(1)
    this%L(:,:)  =  this%L(:,:)   /(2.0_Dbl * c)

    ! Fill eigenvalue matrix
    !this%Gam(1,1) = this%Lambda%U(1)
    !this%Gam(1,2) = 0.0_Dbl
    !this%Gam(2,1) = 0.0_Dbl
    !this%Gam(2,2) = this%Lambda%U(2)

    ! Fill Gamma plus
    !this%Gam_plus(1,1) = dmax1(this%Lambda%U(1), 0.0_Dbl)
    !this%Gam_plus(1,2) = 0.0_Dbl
    !this%Gam_plus(2,1) = 0.0_Dbl
    !this%Gam_plus(2,2) = dmax1(this%Lambda%U(2), 0.0_Dbl)

    ! Fill Gamma minus
    !this%Gam_minus(1,1) = dmin1(this%Lambda%U(1), 0.0_Dbl)
    !this%Gam_minus(1,2) = 0.0_Dbl
    !this%Gam_minus(2,1) = 0.0_Dbl
    !this%Gam_minus(2,2) = dmin1(this%Lambda%U(2), 0.0_Dbl)


    ! Compute A plus
    !this%A_plus  = matmul(matmul(this%R, this%Gam_plus), this%L)

    ! Compute A minus
    !this%A_minus = matmul(matmul(this%R, this%Gam_minus), this%L)

    ! Compute A abs
    !this%A_abs = this%A_plus - this%A_minus
  else
    print*, "Fatal error: the option of the Jacobian interpolation has not been defined."
    stop
  end if


!write(*,       *) " end subroutine < Jacobian_sub >"
!write(FileInfo,*) " end subroutine < Jacobian_sub >"
return
end subroutine Jacobian_sub


!##################################################################################################
! Purpose: This subroutine computes the eiqenvalues of a 2x2 matrix.
!
! Developed by: Babak Poursartip
! Supervised by: Clint Dawson
!
! The Institute for Computational Engineering and Sciences (ICES)
! The University of Texas at Austin
!
! ================================ V E R S I O N ==================================================
! V0.00: 03/19/2018 - Subroutine initiated.
! V0.01: 03/20/2018 - Initiated: Compiled without error for the first time.
!
! File version $Id $
!
! Last update: 03/20/2018
!
! ================================ L O C A L   V A R I A B L E S ==================================
! (Refer to the main code to see the list of imported variables)
!  . . . . . . . . . . . . . . . . Variables . . . . . . . . . . . . . . . . . . . . . . . . . . .
!
!##################################################################################################

pure subroutine Eigenvalues_sub(A, Lambda1, Lambda2)


! Libraries =======================================================================================

! User defined modules ============================================================================

implicit none

! Global variables ================================================================================

! - integer variables -----------------------------------------------------------------------------

! - real variables --------------------------------------------------------------------------------
real(kind=Dbl),     intent(out)    :: Lambda1, Lambda2

! - complex variables -----------------------------------------------------------------------------

! - integer Arrays --------------------------------------------------------------------------------

! - real Arrays -----------------------------------------------------------------------------------
real(kind=Dbl), intent(in), dimension (:,:)  :: A

! - character variables ---------------------------------------------------------------------------
! - logical variables -----------------------------------------------------------------------------

! Local variables =================================================================================
! - integer variables -----------------------------------------------------------------------------
! - real variables --------------------------------------------------------------------------------
real(kind=Dbl)      :: a11, a12, a21, a22

! code ============================================================================================
!write(*,       *) " subroutine < Eigenvalues_sub >: "
!write(FileInfo,*) " subroutine < Eigenvalues_sub >: "

a11 = A (1,1)
a21 = A (2,1)
a12 = A (1,2)
a22 = A (2,2)

Lambda1 = ((a11+a22) + dsqrt((a11+a22)**2 - 4.0_Dbl*(a11*a22-a12*a21)))/2.0_Dbl
Lambda2 = ((a11+a22) - dsqrt((a11+a22)**2 - 4.0_Dbl*(a11*a22-a12*a21)))/2.0_Dbl

!write(*,       *) " end subroutine < Eigenvalues_sub >"
!write(FileInfo,*) " end subroutine < Eigenvalues_sub >"
return
end subroutine Eigenvalues_sub


!##################################################################################################
! Purpose: This subroutine computes the inverse of a 2x2 matrix.
!
! Developed by: Babak Poursartip
! Supervised by: Clint Dawson
!
! The Institute for Computational Engineering and Sciences (ICES)
! The University of Texas at Austin
!
! ================================ V E R S I O N ==================================================
! V0.00: 03/26/2018 - Subroutine initiated.
! V0.01: 03/26/2018 - Initiated: Compiled without error for the first time.
!
! File version $Id $
!
! Last update: 03/26/2018
!
! ================================ L O C A L   V A R I A B L E S ==================================
! (Refer to the main code to see the list of imported variables)
!  . . . . . . . . . . . . . . . . Variables . . . . . . . . . . . . . . . . . . . . . . . . . . .
!
!##################################################################################################

pure subroutine Inverse(Matrix_in, Matrix_out)


! Libraries =======================================================================================

! User defined modules ============================================================================


implicit none

! Global variables ================================================================================

! - integer variables -----------------------------------------------------------------------------
! - real variables --------------------------------------------------------------------------------
! - complex variables -----------------------------------------------------------------------------
! - integer Arrays --------------------------------------------------------------------------------
! - real Arrays -----------------------------------------------------------------------------------
real(kind=Dbl),  intent(in),  dimension (:,:)  :: Matrix_in
real(kind=Dbl),  intent(out), dimension (:,:)  :: Matrix_out


! Local variables =================================================================================
! - integer variables -----------------------------------------------------------------------------
! - real variables --------------------------------------------------------------------------------
real(kind=Dbl) :: determinant
! - real Arrays -----------------------------------------------------------------------------------

! code ============================================================================================
!write(*,       *) " subroutine < Inverse >: "
!write(FileInfo,*) " subroutine < Inverse >: "

! determinant
determinant = Matrix_in(1,1) * Matrix_in(2,2) - Matrix_in(1,2) * Matrix_in(2,1)


Matrix_out(1,1) = +Matrix_in(2,2)
Matrix_out(2,1) = -Matrix_in(2,1)
Matrix_out(1,2) = -Matrix_in(1,2)
Matrix_out(2,2) = +Matrix_in(1,1)

Matrix_out(:,:) = Matrix_out(:,:) / determinant


!write(*,       *) " end subroutine < Inverse >"
!write(FileInfo,*) " end subroutine < Inverse >"
return
end subroutine Inverse

!##################################################################################################
! Purpose: This subroutine simulates the junction in a flow combination.
!          As of now this function simulates the flow combination. In fact, this function, computes
!          the h and uh for the two required ghost cells for each reach.
!          Junction simulation is based on HEC-RAS model.
!
!  flow combination
!  Left Reach  Right Reach
!          \    /
!           \  /
!            \/
!            |
!            |
!        bottom Reach
!
! Developed by: Babak Poursartip
!
! Supervised by: Clint Dawson
! The Institute for Computational Engineering and Sciences (ICES)
! The University of Texas at Austin
!
! ================================ V E R S I O N ==================================================
! V0.00: 09/18/2018 - File initiated.
!
! File version $Id $
!
! Last update: 09/18/2018
!
! ================================ L O C A L   V A R I A B L E S ==================================
! (Refer to the main code to see the list of imported variables)
!  . . . . . . . . . . . . . . . . Variables . . . . . . . . . . . . . . . . . . . . . . . . . . .
!
!##################################################################################################

subroutine Junction_simulation_flow_combination_upstream( &
             Junction_Model, &
             Width_LeftReach, Width_RightReach, Width_BottomReach, &
             ReachLeft_Cell_n, ReachRight_Cell_n, ReachBottom_Cell_1, &
             ReachBottom_Cell_0, ReachBottom_Cell_n1)

! Libraries =======================================================================================

! User defined modules ============================================================================


implicit none

! Global variables ================================================================================
! - integer variables -----------------------------------------------------------------------------
integer(kind=Smll)  :: Junction_Model ! 1 energy based junction method
                                     ! 2 momentum based junction method
real(kind=Dbl) :: Width_LeftReach           ! The width of the left upstream reach

real(kind=Dbl) :: Width_RightReach          ! The width of the left upstream reach

real(kind=Dbl) :: Width_BottomReach           ! The width of the left upstream reach

! - complex variables -----------------------------------------------------------------------------
! - integer Arrays --------------------------------------------------------------------------------
! - real Arrays -----------------------------------------------------------------------------------
!real(kind=Dbl),  intent(in),  dimension (:,:)  ::

! - types -----------------------------------------------------------------------------------------
type(vector), intent(in) :: ReachLeft_Cell_n  ! Cell n of upstream left reach
type(vector), intent(in) :: ReachRight_Cell_n !Cell n of upstream right reach
type(vector), intent(in) :: ReachBottom_Cell_1  !Cell 1 of downstream bottom reach

type(vector), intent(out) :: ReachBottom_Cell_0  !Cell 0 of downstream bottom reach - output
type(vector), intent(out) :: ReachBottom_Cell_n1 !Cell -1 of downstream bottom reach - output

! Local variables =================================================================================
! - integer variables -----------------------------------------------------------------------------
! - real variables --------------------------------------------------------------------------------
! - real variables --------------------------------------------------------------------------------
real(kind=Dbl) :: u_Left, h_Left   ! velocity and height at the bottom cell of the Left reach
                                   ! -upstream, used to calculate the Froude number

real(kind=Dbl) :: u_Right, h_Right ! velocity and height at the bottom cell of the Right reach
                                   ! -upstream, used to calculate the Froude number

real(kind=Dbl) :: u_Bottom, h_Bottom ! velocity and height at the top cell of the bottom reach
                                     ! -downstream, used to calculate the Froude number

real(kind=Dbl) :: FroudeLeft   ! Froude number at the bottom cell of the Left reach-upstream
real(kind=Dbl) :: FroudeRight  ! Froude number at the bottom cell of the Right reach-upstream
real(kind=Dbl) :: FroudeBottom ! Froude number at the top cell of the bottom reach-downstream

! - real Arrays -----------------------------------------------------------------------------------

! code ============================================================================================
! defining the velocity and the height at first/last cells
h_Left   =  ReachLeft_Cell_n%U(1)
u_Left   =  ReachLeft_Cell_n%U(2)/ReachLeft_Cell_n%U(1)

h_Right  = ReachRight_Cell_n%U(1)
u_Right  = ReachRight_Cell_n%U(2)/ReachRight_Cell_n%U(1)

h_Bottom = ReachBottom_Cell_1%U(1)
u_Bottom = ReachBottom_Cell_1%U(2)/ReachBottom_Cell_1%U(1)

! Calculating the flow regime in each reach based on the Froude number
!   if Fr no. in all reaches < 1: sub-critical flow
!   if Fr no. in all reaches > 1: super-critical flow
!   if Fr no. are diff in each reach: mixed regime
FroudeLeft   = u_Left   / dsqrt(Gravity*h_Left)
FroudeRight  = u_Right  / dsqrt(Gravity*h_Right)
FroudeBottom = u_Bottom / dsqrt(Gravity*h_Bottom)

  if (Junction_Model == 1) then ! energy based junction method

    ! As of know we only have the sub-critical option

    ! Final values for the ghost cells:
    ! ghost cells
    ReachBottom_Cell_n1%U(1) = h_Bottom      ! height
    !ReachBottom_Cell_n1%U(1) = max(h_Left,h_Right)  ! height
    ReachBottom_Cell_n1%U(2) = &
         (Width_LeftReach*u_Left*h_Left + Width_RightReach*u_Right*h_Right)/Width_BottomReach  ! uh

    ReachBottom_Cell_0%U(1)  = h_Bottom      ! height
    !ReachBottom_Cell_0%U(1)  = max(h_Left,h_Right)      ! height
    ReachBottom_Cell_0%U(2)  = &
         (Width_LeftReach*u_Left*h_Left + Width_RightReach*u_Right*h_Right)/Width_BottomReach  ! uh

!    ! Indicating the flow regime based on the Froude number- all less than one, sub-critical flow
    if (FroudeLeft < 1.0_dbl .and. FroudeRight < 1.0_dbl .and. FroudeBottom < 1.0_dbl) then
      ! case 1: Subcritical flow
      !print*,"sub-critical flow"


!      ! Final values for the ghost cells:
!      LeftReach%UU(n+1)%U(1)  =
!      LeftReach%UU(n+1)%U(2)  =
!
!      LeftReach%UU(n+2)%U(1)  =
!      LeftReach%UU(n+2)%U(2)  =
!
!      RightReach%UU(n+1)%U(1) =
!      RightReach%UU(n+1)%U(2) =
!
!      RightReach%UU(n+2)%U(1) =
!      RightReach%UU(n+2)%U(2) =
!
!      BottomReach%UU(-1)%U(1) =
!      BottomReach%UU(-1)%U(2) =
!
!      BottomReach%UU(0)%U(1)  =
!      BottomReach%UU(0)%U(2)  =
!
    ! Indicating the flow regime based on the Froude number- all Fr > 1, super-critical flow
    else if (FroudeLeft > 1.0_dbl .and. FroudeRight > 1.0_dbl .and. FroudeBottom > 1.0_dbl) then
      ! case 2: Supercritical flow
      !print*,"super-critical flow"

!      ! Final values for the ghost cells:
!      LeftReach%UU(n+1)%U(1)  =
!      LeftReach%UU(n+1)%U(2)  =
!
!      LeftReach%UU(n+2)%U(1)  =
!      LeftReach%UU(n+2)%U(2)  =
!
!      RightReach%UU(n+1)%U(1) =
!      RightReach%UU(n+1)%U(2) =
!
!      RightReach%UU(n+2)%U(1) =
!      RightReach%UU(n+2)%U(2) =
!
!      BottomReach%UU(-1)%U(1) =
!      BottomReach%UU(-1)%U(2) =
!
!      BottomReach%UU(0)%U(1)  =
!      BottomReach%UU(0)%U(2)  =
!
    else
      ! case 3: Mixed flow regime
      !print*,"mixed regime flow"
!
!      ! Final values for the ghost cells:
!      LeftReach%UU(n+1)%U(1)  =
!      LeftReach%UU(n+1)%U(2)  =
!
!      LeftReach%UU(n+2)%U(1)  =
!      LeftReach%UU(n+2)%U(2)  =
!
!      RightReach%UU(n+1)%U(1) =
!      RightReach%UU(n+1)%U(2) =
!
!      RightReach%UU(n+2)%U(1) =
!      RightReach%UU(n+2)%U(2) =
!
!      BottomReach%UU(-1)%U(1) =
!      BottomReach%UU(-1)%U(2) =
!
!      BottomReach%UU(0)%U(1)  =
!      BottomReach%UU(0)%U(2)  =
!
!
    end if

  else if (Junction_Model == 2) then  ! Momentum based junction method

! <modify>
! this section later. As of now, we only have the energy based option for junction simulation

!    ! Indicating the flow regime based on the Froude number- all less than one, sub-critical flow
!    if (FroudeLeft < 1.0_dbl) .and. (FroudeRight < 1.0_dbl) .and. (FroudeBottom < 1.0_dbl) then
!      ! case 1: Subcritical flow
!
!      ! Final values for the ghost cells:
!      LeftReach%UU(n+1)%U(1)  =
!      LeftReach%UU(n+1)%U(2)  =
!
!      LeftReach%UU(n+2)%U(1)  =
!      LeftReach%UU(n+2)%U(2)  =
!
!      RightReach%UU(n+1)%U(1) =
!      RightReach%UU(n+1)%U(2) =
!
!      RightReach%UU(n+2)%U(1) =
!      RightReach%UU(n+2)%U(2) =
!
!      BottomReach%UU(-1)%U(1) =
!      BottomReach%UU(-1)%U(2) =
!
!      BottomReach%UU(0)%U(1)  =
!      BottomReach%UU(0)%U(2)  =
!
!    ! Indicating the flow regime based on the Froude number- all Fr > 1, super-critical flow
!    else if (FroudeLeft > 1.0_dbl) .and. (FroudeRight > 1.0_dbl).and.(FroudeBottom > 1.0_dbl) then
!      ! case 2: Supercritical flow
!
!      ! Final values for the ghost cells:
!      LeftReach%UU(n+1)%U(1)  =
!      LeftReach%UU(n+1)%U(2)  =
!
!      LeftReach%UU(n+2)%U(1)  =
!      LeftReach%UU(n+2)%U(2)  =
!
!      RightReach%UU(n+1)%U(1) =
!      RightReach%UU(n+1)%U(2) =
!
!      RightReach%UU(n+2)%U(1) =
!      RightReach%UU(n+2)%U(2) =
!
!      BottomReach%UU(-1)%U(1) =
!      BottomReach%UU(-1)%U(2) =
!
!      BottomReach%UU(0)%U(1)  =
!      BottomReach%UU(0)%U(2)  =
!
!    else
!      ! case 3: Mixed flow regime
!
!      ! Final values for the ghost cells:
!      LeftReach%UU(n+1)%U(1)  =
!      LeftReach%UU(n+1)%U(2)  =
!
!      LeftReach%UU(n+2)%U(1)  =
!      LeftReach%UU(n+2)%U(2)  =
!
!      RightReach%UU(n+1)%U(1) =
!      RightReach%UU(n+1)%U(2) =
!
!      RightReach%UU(n+2)%U(1) =
!      RightReach%UU(n+2)%U(2) =
!
!      BottomReach%UU(-1)%U(1) =
!      BottomReach%UU(-1)%U(2) =
!
!      BottomReach%UU(0)%U(1)  =
!      BottomReach%UU(0)%U(2)  =
!
!    end if

  end if

!write(*,       *) " end subroutine < Inverse >"
!write(FileInfo,*) " end subroutine < Inverse >"
return
end subroutine Junction_simulation_flow_combination_upstream


subroutine Junction_simulation_flow_combination_downstream( &
             Junction_Model, &
             Width_LeftReach, Width_BottomReach, &
             ReachLeft_Cell_n, ReachBottom_Cell_1, &
             ReachLeft_Cell_np1, ReachLeft_Cell_np2)

! Libraries =======================================================================================

! User defined modules ============================================================================


implicit none

! Global variables ================================================================================
! - integer variables -----------------------------------------------------------------------------
integer(kind=Smll)  :: Junction_Model ! 1 energy based junction method
                                     ! 2 momentum based junction method
real(kind=Dbl) :: Width_LeftReach           ! The width of the left upstream reach

real(kind=Dbl) :: Width_BottomReach           ! The width of the left upstream reach

! - complex variables -----------------------------------------------------------------------------
! - integer Arrays --------------------------------------------------------------------------------
! - real Arrays -----------------------------------------------------------------------------------

! - types -----------------------------------------------------------------------------------------
type(vector), intent(in) :: ReachLeft_Cell_n  ! Cell n of upstream left reach
type(vector), intent(in) :: ReachBottom_Cell_1  !Cell 1 of downstream bottom reach

type(vector), intent(out) :: ReachLeft_Cell_np1  ! Cell n+1 of upstream left reach - output
type(vector), intent(out) :: ReachLeft_Cell_np2  ! Cell n+2 of upstream left reach - output

! Local variables =================================================================================
! - integer variables -----------------------------------------------------------------------------
! - real variables --------------------------------------------------------------------------------
! - real variables --------------------------------------------------------------------------------
real(kind=Dbl) :: u_Left, h_Left   ! velocity and height at the bottom cell of the Left reach
                                   ! -upstream, used to calculate the Froude number
real(kind=Dbl) :: u_Bottom, h_Bottom ! velocity and height at the top cell of the bottom reach
                                     ! -downstream, used to calculate the Froude number

real(kind=Dbl) :: h_upstream_Left  ! the estimated height of the water at the left upstream reach
                                   ! based on energy/momentum equation.

real(kind=Dbl) :: FroudeLeft   ! Froude number at the bottom cell of the Left reach-upstream
real(kind=Dbl) :: FroudeBottom ! Froude number at the top cell of the bottom reach-downstream

! - real Arrays -----------------------------------------------------------------------------------


! code ============================================================================================

!write(*,       *) " subroutine < Junction_simulation_flow_combination_downstream >: "
!write(FileInfo,*) " subroutine < Junction_simulation_flow_combination_downstream >: "

! defining the velocity and the height at first/last cells
h_Left   =  ReachLeft_Cell_n%U(1)
u_Left   =  ReachLeft_Cell_n%U(2)/ReachLeft_Cell_n%U(1)

h_Bottom = ReachBottom_Cell_1%U(1)
u_Bottom = ReachBottom_Cell_1%U(2)/ReachBottom_Cell_1%U(1)

! Calculating the flow regime in each reach based on the Froude number
!   if Fr no. in all reaches < 1: sub-critical flow
!   if Fr no. in all reaches > 1: super-critical flow
!   if Fr no. are diff in each reach: mixed regime
FroudeLeft   = u_Left   / dsqrt(Gravity*h_Left)
FroudeBottom = u_Bottom / dsqrt(Gravity*h_Bottom)

  if (Junction_Model == 1) then ! energy based junction method

    ! As of know we only have the sub-critical option

    ! computing the height of water at the left upstream reach based on the conservation of energy
    h_upstream_Left = h_Bottom + u_Bottom**2./(2.0_Dbl*Gravity) - u_Left**2./(2.0_Dbl*Gravity)

    ! Final values for the ghost cells:
    ! ghost cells for the left upstream reach
    !ReachLeft_Cell_np1%U(1)  = h_upstream_Left          ! h height
    ReachLeft_Cell_np1%U(1)  = h_Bottom          ! h height
    ReachLeft_Cell_np1%U(2)  = ReachLeft_Cell_n%U(2)    ! uh

    !ReachLeft_Cell_np2%U(1)  = h_upstream_Left          ! height
    ReachLeft_Cell_np2%U(1)  = h_Bottom          ! height
    ReachLeft_Cell_np2%U(2)  = ReachLeft_Cell_n%U(2)    ! uh

!    ! Indicating the flow regime based on the Froude number- all less than one, sub-critical flow
    if (FroudeLeft < 1.0_dbl .and. FroudeBottom < 1.0_dbl) then
      ! case 1: Subcritical flow
      !print*,"sub-critical flow"


!      ! Final values for the ghost cells:
!      LeftReach%UU(n+1)%U(1)  =
!      LeftReach%UU(n+1)%U(2)  =
!
!      LeftReach%UU(n+2)%U(1)  =
!      LeftReach%UU(n+2)%U(2)  =
!
!      RightReach%UU(n+1)%U(1) =
!      RightReach%UU(n+1)%U(2) =
!
!      RightReach%UU(n+2)%U(1) =
!      RightReach%UU(n+2)%U(2) =
!
!      BottomReach%UU(-1)%U(1) =
!      BottomReach%UU(-1)%U(2) =
!
!      BottomReach%UU(0)%U(1)  =
!      BottomReach%UU(0)%U(2)  =
!
    ! Indicating the flow regime based on the Froude number- all Fr > 1, super-critical flow
    else if (FroudeLeft > 1.0_dbl .and. FroudeBottom > 1.0_dbl) then
      ! case 2: Supercritical flow
      !print*,"super-critical flow"

!      ! Final values for the ghost cells:
!      LeftReach%UU(n+1)%U(1)  =
!      LeftReach%UU(n+1)%U(2)  =
!
!      LeftReach%UU(n+2)%U(1)  =
!      LeftReach%UU(n+2)%U(2)  =
!
!      RightReach%UU(n+1)%U(1) =
!      RightReach%UU(n+1)%U(2) =
!
!      RightReach%UU(n+2)%U(1) =
!      RightReach%UU(n+2)%U(2) =
!
!      BottomReach%UU(-1)%U(1) =
!      BottomReach%UU(-1)%U(2) =
!
!      BottomReach%UU(0)%U(1)  =
!      BottomReach%UU(0)%U(2)  =
!
    else
      ! case 3: Mixed flow regime
      !print*,"mixed regime flow"
!
!      ! Final values for the ghost cells:
!      LeftReach%UU(n+1)%U(1)  =
!      LeftReach%UU(n+1)%U(2)  =
!
!      LeftReach%UU(n+2)%U(1)  =
!      LeftReach%UU(n+2)%U(2)  =
!
!      RightReach%UU(n+1)%U(1) =
!      RightReach%UU(n+1)%U(2) =
!
!      RightReach%UU(n+2)%U(1) =
!      RightReach%UU(n+2)%U(2) =
!
!      BottomReach%UU(-1)%U(1) =
!      BottomReach%UU(-1)%U(2) =
!
!      BottomReach%UU(0)%U(1)  =
!      BottomReach%UU(0)%U(2)  =
!
!
    end if

  else if (Junction_Model == 2) then  ! Momentum based junction method

! <modify>
! this section later. As of now, we only have the energy based option for junction simulation

!    ! Indicating the flow regime based on the Froude number- all less than one, sub-critical flow
!    if (FroudeLeft < 1.0_dbl) .and. (FroudeRight < 1.0_dbl) .and. (FroudeBottom < 1.0_dbl) then
!      ! case 1: Subcritical flow
!
!      ! Final values for the ghost cells:
!      LeftReach%UU(n+1)%U(1)  =
!      LeftReach%UU(n+1)%U(2)  =
!
!      LeftReach%UU(n+2)%U(1)  =
!      LeftReach%UU(n+2)%U(2)  =
!
!      RightReach%UU(n+1)%U(1) =
!      RightReach%UU(n+1)%U(2) =
!
!      RightReach%UU(n+2)%U(1) =
!      RightReach%UU(n+2)%U(2) =
!
!      BottomReach%UU(-1)%U(1) =
!      BottomReach%UU(-1)%U(2) =
!
!      BottomReach%UU(0)%U(1)  =
!      BottomReach%UU(0)%U(2)  =
!
!    ! Indicating the flow regime based on the Froude number- all Fr > 1, super-critical flow
!    else if (FroudeLeft > 1.0_dbl) .and. (FroudeRight > 1.0_dbl).and.(FroudeBottom > 1.0_dbl) then
!      ! case 2: Supercritical flow
!
!      ! Final values for the ghost cells:
!      LeftReach%UU(n+1)%U(1)  =
!      LeftReach%UU(n+1)%U(2)  =
!
!      LeftReach%UU(n+2)%U(1)  =
!      LeftReach%UU(n+2)%U(2)  =
!
!      RightReach%UU(n+1)%U(1) =
!      RightReach%UU(n+1)%U(2) =
!
!      RightReach%UU(n+2)%U(1) =
!      RightReach%UU(n+2)%U(2) =
!
!      BottomReach%UU(-1)%U(1) =
!      BottomReach%UU(-1)%U(2) =
!
!      BottomReach%UU(0)%U(1)  =
!      BottomReach%UU(0)%U(2)  =
!
!    else
!      ! case 3: Mixed flow regime
!
!      ! Final values for the ghost cells:
!      LeftReach%UU(n+1)%U(1)  =
!      LeftReach%UU(n+1)%U(2)  =
!
!      LeftReach%UU(n+2)%U(1)  =
!      LeftReach%UU(n+2)%U(2)  =
!
!      RightReach%UU(n+1)%U(1) =
!      RightReach%UU(n+1)%U(2) =
!
!      RightReach%UU(n+2)%U(1) =
!      RightReach%UU(n+2)%U(2) =
!
!      BottomReach%UU(-1)%U(1) =
!      BottomReach%UU(-1)%U(2) =
!
!      BottomReach%UU(0)%U(1)  =
!      BottomReach%UU(0)%U(2)  =
!
!    end if

  end if

!write(*,       *) " end subroutine < Inverse >"
!write(FileInfo,*) " end subroutine < Inverse >"
return
end subroutine Junction_simulation_flow_combination_downstream


end module Solver_mod






