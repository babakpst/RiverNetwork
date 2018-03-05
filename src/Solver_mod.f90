
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
    procedure Solve => Slover_1D_Richtmyer
end type Richtmyer

  interface Solve
    module procedure Slover_1D_Richtmyer
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

subroutine Slover_1D_Richtmyer(                                       &
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
write(*,       *) " subroutine < Slover_1D_Richtmyer >: "
write(FileInfo,*) " subroutine < Slover_1D_Richtmyer >: "





write(*,       *) " end subroutine < Slover_1D_Richtmyer >"
write(FileInfo,*) " end subroutine < Slover_1D_Richtmyer >"
return
end subroutine Slover_1D_Richtmyer

end module Solver_mod

