
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
!
! File version $Id $
!
! Last update: 03/02/2018
!
! ================================ S U B R O U T I N E ============================================
! Slover_1D_Richtmyer: Solves the 1D shallow water equation, using the Richtmyer method.
! Impose_Boundary_Condition_1D_sub: Imposes boundary conditions on the 1D model
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

implicit none

!public
!private

type, public :: Richtmyer

  type(discretization_tp) :: Geometry
  type(AnalysisData_tp)   :: AnalysisInfo

  integer(kind=Lnd) = Plot = 100

  real(kind=DBL)    :: Gravity = 9.81_Dbl
  real(kind=DBL)    :: h(NCells)   ! water height at each step
  real(kind=DBL)    :: uh(NCells)  ! water height*velocity at each step
  real(kind=DBL)    :: hm(NCells)  ! water height at each half step
  real(kind=DBL)    :: uhm(NCells) ! water height*velocity at each half step

  real(kind=DBL)    :: s_0(NCells) ! bottom slope
  real(kind=DBL)    :: s_f(NCells) ! friction slope at each step
  real(kind=DBL)    :: s(NCells)   ! temp
  real(kind=DBL)    :: s_f_m(NCells-1_Lng) ! friction slope at each step
  real(kind=DBL)    :: s_m(NCells-1_Lng-1_Lng)   ! temp

  contains
    procedure Solve => Slover_1D_Richtmyer_sub
    procedure BC => Impose_Boundary_Condition_1D_sub
end type Richtmyer

  interface Solve
    module procedure Slover_1D_Richtmyer_sub
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

subroutine Slover_1D_Richtmyer_sub(                                       &
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
class(Richtmyer) :: this
type(Plot_Results_1D_tp(NCells)) :: Results


! Local variables =================================================================================
! - integer variables -----------------------------------------------------------------------------
integer(kind=Lng)  :: i_steps   ! loop index over the number steps

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
write(*,       *) " subroutine < Slover_1D_Richtmyer_sub >: "
write(FileInfo,*) " subroutine < Slover_1D_Richtmyer_sub >: "

write(*,       *) " -Solving the shallow water equation ..."
write(FileInfo,*) " -Solving the shallow water equation ..."

! Applying initial conditions:
write(*,       *) " -Applying initial conditions ..."
write(FileInfo,*) " -Applying initial conditions ..."

this%h(:) = this%AnalysisInfo%CntrlV
this%uh(:) = 0.0_Dbl

this%Initial()

  do i_steps = 1_Lng, NSteps

    ! write down data for visualization
    call Results%plot_results(i_steps)

    ! applying boundary conditions


    ! find the solution at the half step
    this%s_f(:) = (this%ManningCell(:)**2.0_Dbl)*((this%uh(:)/this%h(:)) &
                  *dabs(this%uh(:)/this%h(:)))/(((this%WidthCell(:)*this%h(:))/(this%WidthCell(:)+2.0_Dbl*this%h(:)) )**(4.0_Dbl/3.0_Dbl))
    this%s(:) = - Gravity * this%h(:)*(this%s_0(:) - this%s_f(:))

    this%hm(1:NCells-1) = (this%h(1:NCells-1) + this%h(2:NCells) ) / 2.0_Dbl - ( dt / 2.0_Dbl ) * (this%uh(2:NCells) - this%uh(1:NCells-1) ) / this%Geometry%LengthCell(1:NCells-1)
    this%uhm(1:NCells-1) = (this%uh(1:NCells-1) + this%uh(2:NCells)) / 2.0_Dbl - ( dt / 2.0_Dbl ) * ( (this%uh(2:NCells) ** 2.0_Dbl) / this%h(2:NCells) + 0.5_Dbl * Gravity * ( this%h(2:NCells) ** 2.0_Dbl) - (this%uh(1:NCells-1)** 2.0_Dbl)/ this%h(1:NCells-1) - 0.5_Dbl * Gravity * (this%h(1:NCells-1) ** 2.0_Dbl )) / this%Geometry%LengthCell(1:NCells-1) - ( dt / 4.0_Dbl ) * ( this%s(2:NCells) + this%s(1:NCells-1) )

    ! find the solution at the full step
    this%S_f_m(1:NCells_1) = (this%Geometry%ManningCell(1:NCells_1) **2.0) * ( (this%uhm(1:NCells_1)/this%hm(1:NCells_1)) * dabs(this%uhm(1:NCells_1)/this%hm(1:NCells_1)) ) / ((( this%Geometry%WidthCell(1:NCells_1) * this%hm(1:NCells_1)) /(this%Geometry%WidthCell(1:NCells_1) + 2.0_Dbl * this%hm(1:NCells_1)) )**(4.0_Dbl/3.0_Dbl))
    this%s_m (1:NCells_1) = - Gravity * this%hm(1:NCells_1)*(this%s_0(1:NCells_1) - this%s_f_m(1:NCells_1))

    this%h(2:NCells_1) = this%h(2:NCells_1) - dt * ( this%uhm(2:NCells_1) - this%uhm(1:NCells-2) ) / this%Geometry%LengthCell(2:NCells_1)
    this%uh(2:NCells_1) = this%uh(2:NCells_1) - dt * ((this%uhm(2:NCells_1) ** 2.0_Dbl)  / this%hm(2:NCells_1) + 0.5_Dbl * Gravity * ( this%hm(2:NCells_1) ** 2.0_Dbl) - (this%uhm(2:NCells-2) ** 2.0_Dbl)  / this%hm(1:NCells-2) - 0.5_Dbl * Gravity * ( this%hm(1:NCells-2) ** 2.0_Dbl) ) / this%Geometry%LengthCell(2:NCells_1) - ( dt / 2.0_Dbl ) * ( this%s_m(2:NCells_1) + this%s_m(1:NCells-2) )

    ! apply boundary condition
    call this%BC()

  end do

write(*,       *) " end subroutine < Slover_1D_Richtmyer_sub >"
write(FileInfo,*) " end subroutine < Slover_1D_Richtmyer_sub >"
return
end subroutine Slover_1D_Richtmyer_sub


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
class(Richtmyer) :: this

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
this%h(NCells)  = h_downstream

this%uh(1) = q_upstream/B
this%uh(NCells)  = this%uh(NCells-1)

write(*,       *) " end subroutine < Impose_Boundary_Condition_1D_sub >"
write(FileInfo,*) " end subroutine < Impose_Boundary_Condition_1D_sub >"
return
end subroutine Impose_Boundary_Condition_1D_sub

end module Solver_mod

