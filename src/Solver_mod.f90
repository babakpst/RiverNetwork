
!##################################################################################################
! Purpose: This module solves the shallow water equation.
!
! Developed by: Babak Poursartip
! Supervised by: Clint Dawson
!
! The Institute for Computational Engineering and Sciences (ICES)
! The University of Texas at Austin
!
! ================================ V E R S I O N ==================================================
! V0.00: 03/02/2018 - File initiated.
! V0.01: 03/02/2018 - Initiated: Compiled without error for the first time.
! V0.10: 03/08/2018 - Initiated: Compiled without error.
!
! File version $Id $
!
! Last update: 03/02/2018
!
! ================================ S U B R O U T I N E ============================================
! Solver_1D_Richtmyer: Solves the 1D shallow water equation, using the Richtmyer method.
! Impose_Boundary_Condition_1D_sub: Imposes boundary conditions on the 1D model
! Solve_1D_SWE_with_Limiter_sub: Solves the 1D SVE with limiter
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
use Results_mod

implicit none

!public
!private

type, public :: Richtmyer(NCells)
  integer(kind=Lng), len :: NCells
  integer(kind=Lng)      :: Plot_Inc = 100

  real(kind=DBL)    :: Gravity = 9.81_Dbl
  real(kind=DBL)    :: h(NCells)   ! water height at each step
  real(kind=DBL)    :: uh(NCells)  ! water height*velocity at each step
  real(kind=DBL)    :: hm(NCells-1_Lng)  ! water height at each half step
  real(kind=DBL)    :: uhm(NCells-1_Lng) ! water height*velocity at each half step

  !real(kind=DBL)    :: s_0(NCells) ! bottom slope
  real(kind=DBL)    :: s_f(NCells) ! friction slope at each step
  real(kind=DBL)    :: s(NCells)   ! temp
  real(kind=DBL)    :: s_f_m(NCells-1_Lng) ! friction slope at each step
  real(kind=DBL)    :: s_m(NCells-1_Lng-1_Lng)   ! temp

  type(discretization_tp) :: Discretization
  type(AnalysisData_tp)   :: AnalysisInfo
  type(Input_Data_tp)     :: ModelInfo
  !type(Plot_domain_1D_tp(NCells)) :: Domain

  contains
    procedure Solve => Solver_1D_Richtmyer_sub
    procedure BC => Impose_Boundary_Condition_1D_sub
end type Richtmyer

  interface Solve
    module procedure Solver_1D_Richtmyer_sub
  end interface

contains

!##################################################################################################
! Purpose: This subroutine solve the 1D shallow water equation, using Richtmyer method.
!
! Developed by: Babak Poursartip
! Supervised by: Clint Dawson
!
! The Institute for Computational Engineering and Sciences (ICES)
! The University of Texas at Austin
!
! ================================ V E R S I O N ==================================================
! V0.00: 03/05/2018 - File initiated.
! V0.01: 03/05/2018 - Initiated: Compiled without error for the first time.
!
! File version $Id $
!
! Last update: 03/05/2018
!
! ================================ L O C A L   V A R I A B L E S ==================================
! (Refer to the main code to see the list of imported variables)
!  . . . . . . . . . . . . . . . . Variables . . . . . . . . . . . . . . . . . . . . . . . . . . .
!
!##################################################################################################

subroutine Solver_1D_Richtmyer_sub(                                       &
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
!type(Plot_Results_1D_tp(NCells = this%NCells)) :: Results
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
write(*,       *) " subroutine < Slover_1D_Richtmyer_sub >: "
write(FileInfo,*) " subroutine < Slover_1D_Richtmyer_sub >: "

write(*,       *) " -Solving the shallow water equation ..."
write(FileInfo,*) " -Solving the shallow water equation ..."

! Applying initial conditions:
write(*,       *) " -Applying initial conditions ..."
write(FileInfo,*) " -Applying initial conditions ..."

allocate(Plot_Results_1D_tp(NCells = this%NCells) :: Results)

this%h(:) = this%AnalysisInfo%CntrlV-    this%Discretization%ZCell(:)
this%uh(:) = 0.0_Dbl

NSteps = this%AnalysisInfo%TotalTime/this%AnalysisInfo%TimeStep
dt     = this%AnalysisInfo%TimeStep
dx     = this%Discretization%LengthCell(1)

PrintResults = .false.

this%s_f(:)   = 0.0_Dbl
this%s(:)     = 0.0_Dbl
this%hm(:)    = 0.0_Dbl
this%uhm(:)   = 0.0_Dbl
this%s_f_m(:) = 0.0_Dbl
this%s_m(:)   = 0.0_Dbl

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
        Results%hm(:)     = this%hm(:)
        Results%uhm(:)    = this%uhm(:)
        Results%s_f_m(:)  = this%s_f_m(:)
        Results%s_m(:)    = this%s_m(:)

        call Results%plot_results(i_steps)
      end if

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

write(*,       *) " end subroutine < Solver_1D_Richtmyer_sub >"
write(FileInfo,*) " end subroutine < Solver_1D_Richtmyer_sub >"
return

end subroutine Solver_1D_Richtmyer_sub


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
! V0.00: 03/05/2018 - File initiated.
! V0.01: 03/05/2018 - Initiated: Compiled without error for the first time.
! V0.10: 03/08/2018 - Initiated: Compiled without error.
!
! File version $Id $
!
! Last update: 03/05/2018
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
this                                                                     & ! type
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
! Purpose: This subroutine solves the 1D Saint Venant Equation (SWE), using a combination of
!          forward/backward Euler (upwind) with Lax-Wendroff method, using various limiters.
!
! Developed by: Babak Poursartip
! Supervised by: Clint Dawson
!
! The Institute for Computational Engineering and Sciences (ICES)
! The University of Texas at Austin
!
! ================================ V E R S I O N ==================================================
! V0.00: 03/09/2018 - Subroutine initiated.
! V0.01: 03/09/2018 - Initiated: Compiled without error for the first time.
!
! File version $Id $
!
! Last update: 03/09/2018
!
! ================================ L O C A L   V A R I A B L E S ==================================
! (Refer to the main code to see the list of imported variables)
!  . . . . . . . . . . . . . . . . Variables . . . . . . . . . . . . . . . . . . . . . . . . . . .
!
!##################################################################################################

subroutine Solve_1D_SWE_with_Limiter_sub(                             &
!                                                                     & ! integer (1) variables
!                                                                     & ! integer (2) variables
!                                                                     & ! integer (4) variables
!                                                                     & ! integer (8) variables
!                                                                     & ! real variables
!                                                                     & ! integer arrays
!                                                                     & ! real arrays
!                                                                     & ! characters
!                                                                     & ! type
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
!#type() ::


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
write(*,       *) " subroutine < Solve_1D_SWE_with_Limiter_sub >: "
write(FileInfo,*) " subroutine < Solve_1D_SWE_with_Limiter_sub >: "


write(*,       *) " end subroutine < Solve_1D_SWE_with_Limiter_sub >"
write(FileInfo,*) " end subroutine < Solve_1D_SWE_with_Limiter_sub >"
return
end subroutine Solve_1D_SWE_with_Limiter_sub






end module Solver_mod

