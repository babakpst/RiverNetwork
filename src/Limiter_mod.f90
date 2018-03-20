
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
! V0.01: 03/20/2018 - Initiated: Compiled without error for the first time.
!
! File version $Id $
!
! Last update: 03/20/2018
!
! ================================ S U B R O U T I N E ============================================
! -Solver_1D_with_Limiter_sub: Solves the 1D shallow water equation, with limiter.
! -Impose_Boundary_Condition_1D_sub: Imposes boundary conditions on the 1D model.
! -Limiters_sub: Contains various limiters.
! -Jacobian_sub: computes the Jacobian matrix, Jacobian plus, and Jacobian minus at each cell.
! -Eigenvalues_sub: computes the eigenvalues of a 2x2 matrix.
!
! ================================ F U N C T I O N ================================================
!
! ================================ L O C A L   V A R I A B L E S ==================================
! (Refer to the main code to see the list of imported variables)
!  . . . . . . . . . . . . . . . . Variables . . . . . . . . . . . . . . . . . . . . . . . . . . .
!
!##################################################################################################

module LaxWendroff_with_limiter_mod

! Libraries =======================================================================================


! User defined modules ============================================================================
use Parameters_mod
use Results_mod

implicit none

!public
!private

type LimiterFunc_tp ! contains all variable to compute the limiter value
  integer(kind=Smll) :: limiter_Type ! Indicates the type of limiter function

  real(kind=Dbl)     :: phi          ! the value of limiter
  real(kind=Dbl)     :: theta          ! the argument of the limiter

  contains
    procedure LimiterValue => Limiters_sub
end type LimiterFunc_tp

! This type consists of all variables/arrays regarding the Jacobian, used to apply the limiter.
type Jacobians_tp
  integer(kind=Tiny) :: option   ! indicates how to interpolate the Jacobian at the interface: 1: for average on the solution-2: direct average on the Jacobian itself

  real(kind=Dbl) :: u_up  ! the velocity at the upstream grid
  real(kind=Dbl) :: u_dw  ! the velocity at the downstream grid

  real(kind=Dbl) :: uh_up  ! the discharge at the upstream grid
  real(kind=Dbl) :: uh_dw  ! the discharge at the downstream grid

  real(kind=Dbl) :: Gravity  ! the ground acceleration

  real(kind=Dbl),dimension(2)   :: Lambda   ! Contains the eigenvalues

  real(kind=Dbl),dimension(2,2) :: A        ! Contains the Jacobian matrix at each time step at the cell interface i-1/2
  real(kind=Dbl),dimension(2,2) :: R        ! Contains the eigenvectors at each time step at the cell interface i-1/2
  real(kind=Dbl),dimension(2,2) :: L        ! Contains the eigenvectors inverse at each time step at the cell interface i-1/2

  real(kind=Dbl),dimension(2,2) :: A_plus   ! Contains the Jacobian matrix with + eigenvalues at each time step at the cell interface i-1/2
  real(kind=Dbl),dimension(2,2) :: A_minus  ! Contains the Jacobian matrix with - eigenvalues at each time step at the cell interface i-1/2
  real(kind=Dbl),dimension(2,2) :: A_abs    ! Contains the Jacobian matrix with abs eigenvalues at each time step at the cell interface i-1/2
  real(kind=Dbl),dimension(2,2) :: Gam      ! Contains the Jacobian matrix with - eigenvalues at each time step at the cell interface i-1/2
  real(kind=Dbl),dimension(2,2) :: Gam_plus ! Contains the Jacobian matrix with - eigenvalues at each time step at the cell interface i-1/2
  real(kind=Dbl),dimension(2,2) :: Gam_minus! Contains the Jacobian matrix with - eigenvalues at each time step at the cell interface i-1/2

  contains
    procedure Jacobian => Jacobian_sub
end type Jacobians_tp

! This vector will be used in the main type as the solution in each type step
type vector
  real(kind=Dbl), dimension(2) :: U
end type U

type, public :: SolverWithLimiter(NCells)
  integer(kind=Lng), len :: NCells
  integer(kind=Lng)      :: Plot_Inc = 100

  real(kind=DBL)    :: Gravity = 9.81_Dbl

  real(kind=DBL),  dimension(NCells) :: s_f ! friction slope at each step

  type(vector),  dimension(NCells) :: s   ! temp to hold bathymetry

  type(vector),  dimension(NCells) :: phi   ! Holds the value of the limiter function <delete>
  type(vector),  dimension(NCells) :: theta ! Holds the value of the limiter function <delete>

  type(vector), dimension(NCells)  :: U     ! This vector holds the solution at the current step,
                                            ! the first term holds "h" and the second holds "uh"

  type(discretization_tp) :: Discretization ! Contains the discretization of the domain
  type(AnalysisData_tp)   :: AnalysisInfo   ! Holds information for the analysis
  type(Input_Data_tp)     :: ModelInfo      ! Holds information for the model

  contains
    procedure Solve => Solver_1D_with_Limiter_sub
    procedure BC => Impose_Boundary_Condition_1D_sub
end type SolverWithLimiter

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
! V0.01: 03/19/2018 - Initiated: Compiled without error for the first time.
!
! File version $Id $
!
! Last update: 03/19/2018
!
! ================================ L O C A L   V A R I A B L E S ==================================
! (Refer to the main code to see the list of imported variables)
!  . . . . . . . . . . . . . . . . Variables . . . . . . . . . . . . . . . . . . . . . . . . . . .
!
!##################################################################################################

subroutine Solver_1D_with_Limiter_sub(this)

! Libraries =======================================================================================

! User defined modules ============================================================================

implicit none

! Global variables ================================================================================

! - integer variables -----------------------------------------------------------------------------
!#integer (kind=Shrt), intent(in)    ::
!#integer (kind=Shrt), intent(inout) ::
!#integer (kind=Shrt), intent(out)   ::
! - real variables --------------------------------------------------------------------------------
!#real (kind=Dbl),     intent(in)    ::
!#real (kind=Dbl),     intent(inout) ::
!#real (kind=Dbl),     intent(out)   ::
! - complex variables -----------------------------------------------------------------------------
!#complex,             intent(in)    ::
!#complex,             intent(inout) ::
!#complex,             intent(out)   ::
! - integer Arrays --------------------------------------------------------------------------------
!#integer (kind=Shrt), intent(in),    dimension (:  )  ::
!#integer (kind=Shrt), intent(in),    dimension (:,:)  ::
!#integer (kind=Shrt), intent(in)    ::
!#integer (kind=Shrt), intent(inout) ::
!#integer (kind=Shrt), intent(out)   ::
! - real Arrays -----------------------------------------------------------------------------------
!#real (kind=Dbl),     intent(in),    dimension (:  )  ::
!#real (kind=Dbl),     intent(inout), dimension (:  )  ::
!#real (kind=Dbl),     intent(out),   dimension (:  )  ::
! - character variables ---------------------------------------------------------------------------
!#character (kind = ?, Len = ? ) ::
! - logical variables -----------------------------------------------------------------------------
!#logical   ::
! - types -----------------------------------------------------------------------------------------
class(SolverWithLimiter(*)) :: this

type(Jacobians_tp)     :: Jacobian
type(LimiterFunc_tp)   :: LimiterFunc
type(Plot_Results_1D_limiter_tp(NCells = :)), allocatable :: Results

! Local variables =================================================================================
! - integer variables -----------------------------------------------------------------------------

integer(kind=Lng)  :: i_steps   ! loop index over the number steps
integer(kind=Lng)  :: NSteps    ! Total number of steps for time marching

! - real variables --------------------------------------------------------------------------------
real(kind=Dbl)      :: dt
real(kind=Dbl)      :: dx

! - complex variables -----------------------------------------------------------------------------
!#complex              ::
! - integer Arrays --------------------------------------------------------------------------------
!#integer (kind=Shrt), dimension (:)  ::
!#integer (kind=Shrt), Allocatable, dimension (:)  ::

! - real Arrays -----------------------------------------------------------------------------------
real(kind=Dbl), dimension (2) :: F_L ! Contribution of low-resolution method (Upwind) in the solution.
real(kind=Dbl), dimension (2) :: F_H ! Contribution of high-resolution method (Lax-Wendroff) in the solution.

! - character variables ---------------------------------------------------------------------------
!#character (kind = ?, Len = ? ) ::
! - logical variables -----------------------------------------------------------------------------
logical   :: PrintResults
! - type ------------------------------------------------------------------------------------------

! code ============================================================================================
write(*,       *) " subroutine < Solver_1D_with_Limiter_sub >: "
write(FileInfo,*) " subroutine < Solver_1D_with_Limiter_sub >: "

write(*,       *) " -Solving the shallow water equation with a limiter ..."
write(FileInfo,*) " -Solving the shallow water equation with a limiter ..."

! Applying initial conditions:
write(*,       *) " -Applying initial conditions ..."
write(FileInfo,*) " -Applying initial conditions ..."

allocate(Plot_Results_1D_tp(NCells = this%NCells) :: Results)

this%U(:)%U(1) = this%AnalysisInfo%CntrlV-    this%Discretization%ZCell(:)
this%U(:)%U(2) = 0.0_Dbl

NSteps = this%AnalysisInfo%TotalTime/this%AnalysisInfo%TimeStep
dt     = this%AnalysisInfo%TimeStep
dx     = this%Discretization%LengthCell(1)

coe = dt / dx

! <modify>
Jacobian%option = 1

! Initialization
LimiterFunc%limiter_Type = this%AnalysisInfo%limiter ! Define what limiter to use in the algorithm
PrintResults = .false.

this%s_f(:)%U(:)   = 0.0_Dbl
this%s(:)%U(:)     = 0.0_Dbl

call this%BC()
Results%ModelInfo = this%ModelInfo

  ! Time marching
  do i_steps = 1_Lng, NSteps

    print*, i_steps*dt

    ! write down data for visualization
      if (mod(i_steps,this%Plot_Inc)==1 .or. PrintResults) then
        Results%U(:)      = this%U(:)

        Results%s_f(:)    = this%s_f(:)
        Results%s(:)      = this%s(:)

        Results%phi(:)  = this%phi(:)
        Results%theta(:)= this%theta(:)

        call Results%plot_results(i_steps)
      end if

      do i_Cell = 2_Lng, NCells-1  ! Loop over the cells except the boundary cells.

        ! Computing the Jacobian and all other items at the upstream

        call Jacobian%Jacobian()   ! <modify>

        ! The limiter function
        LimiterFunc%theta = ! <modify>



        ! The upwind flux
        F_L(:) = 0.0_Dbl

          ! option 1: average the solution

          ! option 2: average the Jacobian



        F_L(:) = F_L(:) +   ! the minus part
        F_L(:) = F_L(:) +   ! the plus part

        ! The Lax-Wendroff flux
        F_H(:)

        ! Update the results  ! <modify> this part. delete the U_Update
        this%U(i_cell) =  this%U(i_cell) - coe * F_L(:)  - coe * F_H(:)


        this%h(2:this%NCells-1)  = this%h(2:this%NCells-1)  +
        this%uh(2:this%NCells-1) = this%uh(2:this%NCells-1) +

      end do









!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! find the solution at the half step
    this%s_f(:) = (this%Discretization%ManningCell(:)**2.0_Dbl)*((this%uh(:)/this%h(:)) *dabs(this%uh(:)/this%h(:)))/ (((this%Discretization%WidthCell(:)*this%h(:))/(this%Discretization%WidthCell(:)+2.0_Dbl*this%h(:)) )**(4.0_Dbl/3.0_Dbl))
    this%s(:) = - this%Gravity * this%h(:)*(this%Discretization%SlopeCell(:) - this%s_f(:))

    !this%hm(1:this%NCells-1)  = (this%h(1:this%NCells-1)  + this%h(2:this%NCells) ) / 2.0_Dbl - ( dt / 2.0_Dbl ) * (this%uh(2:this%NCells) - this%uh(1:this%NCells-1) ) / this%Discretization%LengthCell(1:this%NCells-1)
    !this%uhm(1:this%NCells-1) = (this%uh(1:this%NCells-1) + this%uh(2:this%NCells)) / 2.0_Dbl - ( dt / 2.0_Dbl ) * ((this%uh(2:this%NCells) ** 2.0_Dbl) / this%h(2:this%NCells) + 0.5_Dbl * this%Gravity * ( this%h(2:this%NCells) ** 2.0_Dbl) - (this%uh(1:this%NCells-1)** 2.0_Dbl)/ this%h(1:this%NCells-1) - 0.5_Dbl * this%Gravity * (this%h(1:this%NCells-1) ** 2.0_Dbl )) / this%Discretization%LengthCell(1:this%NCells-1) - ( dt / 4.0_Dbl ) * ( this%s(2:this%NCells) + this%s(1:this%NCells-1) )

    this%hm(1:this%NCells-1)  = (this%h(1:this%NCells-1)  + this%h(2:this%NCells) ) / 2.0_Dbl - ( dt / 2.0_Dbl ) * (this%uh(2:this%NCells) - this%uh(1:this%NCells-1) ) / dx
    this%uhm(1:this%NCells-1) = (this%uh(1:this%NCells-1) + this%uh(2:this%NCells)) / 2.0_Dbl &
                                - (dt/(2.0_Dbl*dx) ) * ((this%uh(2:this%NCells) ** 2.0_Dbl)/this%h(2:this%NCells)+0.5_Dbl*this%Gravity * ( this%h(2:this%NCells) ** 2.0_Dbl) - (this%uh(1:this%NCells-1)** 2.0_Dbl)/this%h(1:this%NCells-1) - 0.5_Dbl * this%Gravity*(this%h(1:this%NCells-1) ** 2.0_Dbl ))  &
                                -  ( dt / 4.0_Dbl ) * ( this%s(2:this%NCells) + this%s(1:this%NCells-1) )

    ! find the solution at the full step
    this%S_f_m(1:this%NCells-1) = (this%Discretization%ManningCell(1:this%NCells-1) **2.0) * ( (this%uhm(1:this%NCells-1)/this%hm(1:this%NCells-1)) * dabs(this%uhm(1:this%NCells-1)/this%hm(1:this%NCells-1)) ) / ((( this%Discretization%WidthCell(1:this%NCells-1) * this%hm(1:this%NCells-1)) /(this%Discretization%WidthCell(1:this%NCells-1) + 2.0_Dbl * this%hm(1:this%NCells-1)) )**(4.0_Dbl/3.0_Dbl))
    this%s_m (1:this%NCells-1) = - this%Gravity * this%hm(1:this%NCells-1)*(this%Discretization%SlopeCell(1:this%NCells-1) - this%s_f_m(1:this%NCells-1))

    !this%h(2:this%NCells-1) = this%h(2:this%NCells-1) - dt * ( this%uhm(2:this%NCells-1) - this%uhm(1:this%NCells-2) ) / this%Discretization%LengthCell(2:this%NCells-1)
    !this%uh(2:this%NCells-1) = this%uh(2:this%NCells-1) - dt * ((this%uhm(2:this%NCells-1) ** 2.0_Dbl)  / this%hm(2:this%NCells-1) + 0.5_Dbl * this%Gravity * ( this%hm(2:this%NCells-1) ** 2.0_Dbl) - (this%uhm(1:this%NCells-2) ** 2.0_Dbl)  / this%hm(1:this%NCells-2) - 0.5_Dbl * this%Gravity * ( this%hm(1:this%NCells-2) ** 2.0_Dbl) ) / this%Discretization%LengthCell(1:this%NCells-2) - ( dt / 2.0_Dbl ) * ( this%s_m(2:this%NCells-1) + this%s_m(1:this%NCells-2) )

    this%h(2:this%NCells-1) = this%h(2:this%NCells-1)   - dt * ( this%uhm(2:this%NCells-1) - this%uhm(1:this%NCells-2) ) / dx
    this%uh(2:this%NCells-1) = this%uh(2:this%NCells-1) - dt * ((this%uhm(2:this%NCells-1) ** 2.0_Dbl)  / this%hm(2:this%NCells-1) + 0.5_Dbl * this%Gravity * ( this%hm(2:this%NCells-1) ** 2.0_Dbl) - (this%uhm(1:this%NCells-2) ** 2.0_Dbl)  / this%hm(1:this%NCells-2) - 0.5_Dbl * this%Gravity * ( this%hm(1:this%NCells-2) ** 2.0_Dbl) ) / dx - ( dt / 2.0_Dbl ) * ( this%s_m(2:this%NCells-1) + this%s_m(1:this%NCells-2) )

    ! apply boundary condition
    call this%BC()

  end do

write(*,       *) " end subroutine < Solver_1D_with_Limiter_sub >"
write(FileInfo,*) " end subroutine < Solver_1D_with_Limiter_sub >"
return

end subroutine Solver_1D_with_Limiter_sub


!##################################################################################################
! Purpose: This subroutine imposes the boundary conditions on a 1D domain.
!
! Developed by: Babak Poursartip
! Supervised by: Clint Dawson
!
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

subroutine Impose_Boundary_Condition_1D_sub(                                                       &
!                                                                     & ! integer (1) variables
!                                                                     & ! integer (2) variables
!                                                                     & ! integer (4) variables
!                                                                     & ! integer (8) variables
!                                                                     & ! real variables
!                                                                     & ! integer arrays
!                                                                     & ! real arrays
!                                                                     & ! characters
this                                                                  & ! type
)


! Libraries =======================================================================================

! User defined modules ============================================================================


implicit none

! Global variables ================================================================================

! - integer variables -----------------------------------------------------------------------------
!#integer (kind=Shrt), intent(in)    ::
!#integer (kind=Shrt), intent(inout) ::
!#integer (kind=Shrt), intent(out)   ::
! - real variables --------------------------------------------------------------------------------
!#real (kind=Dbl),     intent(in)    ::
!#real (kind=Dbl),     intent(inout) ::
!#real (kind=Dbl),     intent(out)   ::
! - complex variables -----------------------------------------------------------------------------
!#complex,             intent(in)    ::
!#complex,             intent(inout) ::
!#complex,             intent(out)   ::
! - integer Arrays --------------------------------------------------------------------------------
!#integer (kind=Shrt), intent(in),    dimension (:  )  ::
!#integer (kind=Shrt), intent(in),    dimension (:,:)  ::
!#integer (kind=Shrt), intent(in)    ::
!#integer (kind=Shrt), intent(inout) ::
!#integer (kind=Shrt), intent(out)   ::
! - real Arrays -----------------------------------------------------------------------------------
!#real (kind=Dbl),     intent(in),    dimension (:  )  ::
!#real (kind=Dbl),     intent(inout), dimension (:  )  ::
!#real (kind=Dbl),     intent(out),   dimension (:  )  ::
! - character variables ---------------------------------------------------------------------------
!#character (kind = ?, Len = ? ) ::
! - logical variables -----------------------------------------------------------------------------
!#logical   ::
! - types -----------------------------------------------------------------------------------------
class(SolverWithLimiter(*)) :: this


! Local variables =================================================================================
! - integer variables -----------------------------------------------------------------------------
!#integer (kind=Shrt)  ::
! - real variables --------------------------------------------------------------------------------
!#real (kind=Dbl)      ::
! - complex variables -----------------------------------------------------------------------------
!#complex              ::
! - integer Arrays --------------------------------------------------------------------------------
!#integer (kind=Shrt), dimension (:)  ::
!#integer (kind=Shrt), Allocatable, dimension (:)  ::
! - real Arrays -----------------------------------------------------------------------------------
!#real (kind=Dbl), dimension (:)      ::
!#real (kind=Dbl), allocatable, dimension (:)  ::
! - character variables ---------------------------------------------------------------------------
!#character (kind = ?, Len = ? ) ::
! - logical variables -----------------------------------------------------------------------------
!#logical   ::
! - type ------------------------------------------------------------------------------------------

! code ============================================================================================
write(*,       *) " subroutine < Impose_Boundary_Condition_1D_sub >: "
write(FileInfo,*) " subroutine < Impose_Boundary_Condition_1D_sub >: "


! Boundary conditions on the height
this%U(1)%U(1) = this%U(2)%U(1)   ! h at the upstream
this%U(this%NCells)%U(1) = this%AnalysisInfo%h_dw ! h at the downstream

! Boundary conditions on the discharge
this%U(1)%U(2)           = this%AnalysisInfo%Q_Up/ this%Discretization%WidthCell(1)  ! h at the upstream
this%U(this%NCells)%U(2) = this%uh(this%NCells-1) ! h at the downstream

write(*,       *) " end subroutine < Impose_Boundary_Condition_1D_sub >"
write(FileInfo,*) " end subroutine < Impose_Boundary_Condition_1D_sub >"
return
end subroutine Impose_Boundary_Condition_1D_sub




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
class(LimiterFunc_tp) :: this

! Local variables =================================================================================

! code ============================================================================================
write(*,       *) " subroutine < Limiters_sub >: "
write(FileInfo,*) " subroutine < Limiters_sub >: "

  select case (this%limiter_Type)

    case(1)  ! minmod: most diffusive
      this%phi = dmax1(  0.0_Dbl, dmin1( 1.0_Dbl, this%theta )  )
    case(2)  !superbee
      this%phi = dmax1( 0.0_Dbl, dmin1( 1.0_Dbl, 2.0_Dbl*this%theta ), dmin1( 2.0_Dbl, this%theta))
    case(3)  ! MC (Woodward)
      this%phi = dmax1( 0.0_Dbl, dmin1( (1.0_Dbl + this%theta)/2.0_Dbl, 2.0_Dbl, 2.0_Dbl*this%theta ) )
    case(4)  ! van Leer
      this%phi = ( this%theta +  dabs(this%theta) ) / ( 1.0_Dbl + dabs(this%theta) )
    case default
      write(*,*)" The limiter type does exist, please select a limiter from the list and modify the input file."
      write(FileInfo,*)" The limiter type does exist, please select a limiter from the list and modify the input file."
      write(*,*)
      write(FileInfo,*)
      write(*,*)" Simulation terminated with error."
      write(*, Fmt_End);  read(*,*);   stop;

  end select


write(*,       *) " end subroutine < Limiters_sub >"
write(FileInfo,*) " end subroutine < Limiters_sub >"
return
end subroutine Limiters_sub


!##################################################################################################
! Purpose: This subroutine computes the Jacobian matrix, Jacobian plus, and Jacobian minus at each cell.
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

subroutine Jacobian_sub(this)


! Libraries =======================================================================================

! User defined modules ============================================================================


implicit none

! Global variables ================================================================================
! - types -----------------------------------------------------------------------------------------
class(Jacobians_tp) :: this

! Local variables =================================================================================
real(kind=Dbl) :: u_ave   ! the average velocity at the interface
real(kind=Dbl) :: uh_ave  ! the average discharge at the interface

real(kind=Dbl), dimension(2,2) :: A_up  ! the average discharge at the interface
real(kind=Dbl), dimension(2,2) :: A_dw  ! the average discharge at the interface

! code ============================================================================================
write(*,       *) " subroutine < Jacobian_sub >: "
write(FileInfo,*) " subroutine < Jacobian_sub >: "

  if (this%option == 1 ) then  ! find the average solution at the interface and then compute the Jacobian

    u_ave  = 0.5_Dbl( this%u_up + this%u_dw )   ! <modify> for unstructured discretization
    uh_ave = 0.5_Dbl( this%uh_up + this%uh_dw ) ! <modify> for unstructured discretization

    h_ave = uh_ave/ u_ave

    c = dsqrt (this%Gravity * h_ave) ! wave speed

    ! Computing the Jacobian - A
    this%A(1,1) = 0.0_Dbl
    this%A(1,2) = 1.0_Dbl
    this%A(2,1) = this%Gravity * h_ave - u_ave**2
    this%A(2,2) = 2.0_Dbl * u_ave

    ! Computing the eigenvalues
    this%Lambda(1) = u_ave - dsqrt(this%Gravity *  h_ave)
    this%Lambda(2) = u_ave + dsqrt(this%Gravity *  h_ave)

    ! Computing the eigenvectors
    this%R(1,1) = 1.0_Dbl
    this%R(1,2) = this%Lambda(1)

    this%R(1,2) = 1.0_Dbl
    this%R(2,2) = this%Lambda(2)

    ! Computing the eigenvectors inverse
    this%RI(1,1) = this%Lambda(2)
    this%RI(1,2) = -1.0_Dbl

    this%RI(2,1) = -this%Lambda(1)
    this%RI(2,2) = 1.0_Dbl

    this%RI(:,:)  =  this%RI(:,:)   /(2.0_Dbl * c)

    ! Fill eigenvalue matrix
    this%Gam(1,1) = this%Lambda(1)
    this%Gam(1,2) = 0.0_Dbl
    this%Gam(2,1) = 0.0_Dbl
    this%Gam(2,2) = this%Lambda(2)

    ! Fill Gamma plus
    this%Gam(1,1) = dmax1(this%Lambda(1), 0.0_Dbl)
    this%Gam(1,2) = 0.0_Dbl
    this%Gam(2,1) = 0.0_Dbl
    this%Gam(2,2) = dmax1(this%Lambda(2), 0.0_Dbl)

    ! Fill Gamma plus
    this%Gam(1,1) = dmin1(this%Lambda(1), 0.0_Dbl)
    this%Gam(1,2) = 0.0_Dbl
    this%Gam(2,1) = 0.0_Dbl
    this%Gam(2,2) = dmin1(this%Lambda(2), 0.0_Dbl)

    ! Compute A plus
    this%A_plus  = matmul(matmul(this%R, this%Gam_plus), this%RI)

    ! Compute A minus
    this%A_minus = matmul(matmul(this%R, this%Gam_minus), this%RI)

    ! Compute A abs
    this%A_abs = this%A_plus - this%A_minus


  else if (this%option == 2 ) then  ! find the Jacobian at each grid and average the Jacobian to find the Jacobian at the interface

    h_up = this%uh_up / this%u_up

    ! Computing the Jacobian at the upstream - A
    A_up(1,1) = 0.0_Dbl
    A_up(1,2) = 1.0_Dbl
    A_up(2,1) = this%Gravity * h_up - this%u_up**2
    A_up(2,2) = 2.0_Dbl * this%u_up

    h_dw = this%uh_dw / this%u_dw

    ! Computing the Jacobian at the upstream - A
    A_dw(1,1) = 0.0_Dbl
    A_dw(1,2) = 1.0_Dbl
    A_dw(2,1) = this%Gravity * h_dw - this%u_dw**2
    A_dw(2,2) = 2.0_Dbl * this%u_dw

    ! average Jacobian
    this%A(:,:) = 0.5_Dbl * ( A_up(:,:) + A_dw(:,:) )



    c = dsqrt (this%Gravity * h_ave) ! wave speed


    ! Computing the eigenvalues
    this%Lambda(1) = u_ave - dsqrt(this%Gravity *  h_ave)
    this%Lambda(2) = u_ave + dsqrt(this%Gravity *  h_ave)

    ! Computing the eigenvectors
    this%R(1,1) = 1.0_Dbl
    this%R(1,2) = this%Lambda(1)

    this%R(1,2) = 1.0_Dbl
    this%R(2,2) = this%Lambda(2)

    ! Computing the eigenvectors inverse
    this%RI(1,1) = this%Lambda(2)
    this%RI(1,2) = -1.0_Dbl

    this%RI(2,1) = -this%Lambda(1)
    this%RI(2,2) = 1.0_Dbl

    this%RI(:,:)  =  this%RI(:,:)   /(2.0_Dbl * c)

    ! Fill eigenvalue matrix
    this%Gam(1,1) = this%Lambda(1)
    this%Gam(1,2) = 0.0_Dbl
    this%Gam(2,1) = 0.0_Dbl
    this%Gam(2,2) = this%Lambda(2)

    ! Fill Gamma plus
    this%Gam(1,1) = dmax1(this%Lambda(1), 0.0_Dbl)
    this%Gam(1,2) = 0.0_Dbl
    this%Gam(2,1) = 0.0_Dbl
    this%Gam(2,2) = dmax1(this%Lambda(2), 0.0_Dbl)

    ! Fill Gamma plus
    this%Gam(1,1) = dmin1(this%Lambda(1), 0.0_Dbl)
    this%Gam(1,2) = 0.0_Dbl
    this%Gam(2,1) = 0.0_Dbl
    this%Gam(2,2) = dmin1(this%Lambda(2), 0.0_Dbl)

    ! Compute A plus
    this%A_plus  = matmul(matmul(this%R, this%Gam_plus), this%RI)

    ! Compute A minus
    this%A_minus = matmul(matmul(this%R, this%Gam_minus), this%RI)

    ! Compute A abs
    this%A_abs = this%A_plus - this%A_minus



  end if







write(*,       *) " end subroutine < Jacobian_sub >"
write(FileInfo,*) " end subroutine < Jacobian_sub >"
return
end subroutine Jacobian_sub


!##################################################################################################
! Purpose: This subroutine computes the eiqenvalues of a 2x2 matrix.
!
! Developed by: Babak Poursartip
! Supervised by:
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

subroutine Eigenvalues_sub(A, Lambda1, Lambda2)


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
real(kind=Dbl)      :: a, b, c, d

! code ============================================================================================
write(*,       *) " subroutine < Eigenvalues_sub >: "
write(FileInfo,*) " subroutine < Eigenvalues_sub >: "

a = A (1,1)
b = A (1,2)
c = A (2,1)
d = A (2,2)

Lambda1 = ((a+d) + dsqrt((a+d)**2 - 4.0_Dbl*(a*d-b*c)))/2.0_Dbl
Lambda2 = ((a+d) - dsqrt((a+d)**2 - 4.0_Dbl*(a*d-b*c)))/2.0_Dbl

write(*,       *) " end subroutine < Eigenvalues_sub >"
write(FileInfo,*) " end subroutine < Eigenvalues_sub >"
return
end subroutine Eigenvalues_sub

end module LaxWendroff_with_limiter_mod

