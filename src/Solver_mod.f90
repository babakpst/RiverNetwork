
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
    procedure Initial => Impose_Boundary_Condition_1D_sub
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


    ! applying boundary conditions

    ! find the solution at the half step

    ! find the solution at the full step




            S_f[0:nx] = (n_manning**2.0) * ( (uh[0:nx]/h[0:nx]) * abs(uh[0:nx]/h[0:nx]) ) / (((B * h[0:nx]) /(B + 2.0 * h[0:nx]) )**(4.0/3.0) )
            S  [0:nx] = - g * h[0:nx] * ( S_0[0:nx] - S_f[0:nx] )

            #for i in range(nx):
            #    print("{:5d},  S_f: {:20.15f},  S: {:20.15f},  h: {:20.15f}".format(i,S_f[i], S[i], h[i]) )
            #print(" This is the end ----------------")

            hm[0:nx-1] = ( h[0:nx-1] + h[1:nx] ) / 2.0 - ( dt / 2.0 ) * ( uh[1:nx] - uh[0:nx-1] ) / dx
            uhm[0:nx-1] = ( uh[0:nx-1] + uh[1:nx] ) / 2.0 - ( dt / 2.0 ) * (  (uh[1:nx] ** 2.0) / h[1:nx]   + 0.5 * g * (h[1:nx] ** 2.0) - (uh[0:nx-1] ** 2.0 )/ h[0:nx-1] - 0.5 * g * (h[0:nx-1] ** 2.0 )) / dx - ( dt / 4.0 ) * ( S[1:nx] + S[0:nx-1] )
            #print(uhm)
            #print(hm)


            S_f_m[0:nx-1] = (n_manning**2.0) * ( (uhm[0:nx-1]/hm[0:nx-1]) * abs(uhm[0:nx-1]/hm[0:nx-1]) ) / (  ( (B * hm[0:nx-1]) /(B + 2 * hm[0:nx-1]) )**(4.0/3.0) )
            S_m  [0:nx-1] = - g * hm[0:nx-1] * ( S_0[0:nx-1] - S_f_m[0:nx-1] )

            h[1:nx-1] = h[1:nx-1] - dt * ( uhm[1:nx-1] - uhm[0:nx-2] ) / dx
            uh[1:nx-1] = uh[1:nx-1] - dt * ( (uhm[1:nx-1] ** 2.0)  / hm[1:nx-1] + 0.5 * g * (hm[1:nx-1] ** 2.0) - (uhm[0:nx-2] ** 2.0)  / hm[0:nx-2] - 0.5 * g * (hm[0:nx-2] ** 2.0) ) / dx - ( dt / 2.0 ) * ( S_m[1:nx-1] + S_m[0:nx-2] )


            h, uh = boundary_conditions ( nx, nt, h, uh, t[it], h_downstream, Q_upstream, B )



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

