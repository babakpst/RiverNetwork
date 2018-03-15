
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
! V0.01: 03/15/2018 - Initiated: Compiled without error for the first time.
!
! File version $Id $
!
! Last update: 03/15/2018
!
! ================================ S U B R O U T I N E ============================================
! Solver_1D_with_Limiter_sub: Solves the 1D shallow water equation, with limiter.
! Impose_Boundary_Condition_1D_sub: Imposes boundary conditions on the 1D model.
! Limiters_sub: Contains various limiters.
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
end type LimiterFunc_tp


type, public :: SolverWithLimiter(NCells)
  integer(kind=Lng), len :: NCells
  integer(kind=Lng)      :: Plot_Inc = 100

  real(kind=DBL)    :: Gravity = 9.81_Dbl
  real(kind=DBL)    :: h(NCells)   ! water height at each step
  real(kind=DBL)    :: uh(NCells)  ! water height*velocity at each step
  real(kind=DBL)    :: hm(NCells-1_Lng)  ! water height at each half step
  real(kind=DBL)    :: uhm(NCells-1_Lng) ! water height*velocity at each half step

  real(kind=DBL)    :: s_f(NCells) ! friction slope at each step
  real(kind=DBL)    :: s(NCells)   ! temp
  real(kind=DBL)    :: s_f_m(NCells-1_Lng) ! friction slope at each step
  real(kind=DBL)    :: s_m(NCells-1_Lng)   ! temp

  type(discretization_tp) :: Discretization
  type(AnalysisData_tp)   :: AnalysisInfo
  type(Input_Data_tp)     :: ModelInfo

  contains
    procedure Solve => Solver_1D_with_Limiter_sub
    procedure Limiter => Limiter_Sub
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

subroutine Solver_1D_with_Limiter_sub(                                &
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

type(LimiterFunc_tp)   :: LimiterFunc
type(Plot_Results_1D_tp(NCells = :)), allocatable :: Results


! Local variables =================================================================================
! - integer variables -----------------------------------------------------------------------------
integer(kind=Lng)  :: i_steps   ! loop index over the number steps
integer(kind=Lng)  :: NSteps    ! Total number of steps for time marching

! - real variables --------------------------------------------------------------------------------
real (kind=Dbl)      :: dt
real (kind=Dbl)      :: dx

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

this%h(:) = this%AnalysisInfo%CntrlV-    this%Discretization%ZCell(:)
this%uh(:) = 0.0_Dbl

NSteps = this%AnalysisInfo%TotalTime/this%AnalysisInfo%TimeStep
dt     = this%AnalysisInfo%TimeStep
dx     = this%Discretization%LengthCell(1)


! Initialization
LimiterFunc%limiter_Type = !<modify>
PrintResults = .false.

this%s_f(:)   = 0.0_Dbl
this%s(:)     = 0.0_Dbl


call this%BC()
Results%ModelInfo = this%ModelInfo

  do i_steps = 1_Lng, NSteps

    print*, i_steps*dt
    ! write down data for visualization


      if (mod(i_steps,this%Plot_Inc)==1 .or. PrintResults) then
        Results%h(:)      = this%h(:)
        Results%uh(:)     = this%uh(:)
        Results%s_f(:)    = this%s_f(:)
        Results%s(:)      = this%s(:)

        call Results%plot_results(i_steps)
      end if







    LimiterFunc%theta = ! <modify>


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
class(Richtmyer(*)) :: this

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

this%h(1) = this%h(2)
this%h(this%NCells)  = this%AnalysisInfo%h_dw

this%uh(1) = this%AnalysisInfo%Q_Up/ this%Discretization%WidthCell(1)
this%uh(this%NCells)  = this%uh(this%NCells-1)

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





end module LaxWendroff_with_limiter_mod

