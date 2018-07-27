

!##################################################################################################
! Purpose: This module computes the elapsed time, mostly to measure speedup.
!
! Developed by: Babak Poursartip
! Supervised by: Clint Dawson
!
! The Institute for Computational Engineering and Sciences (ICES)
! The University of Texas at Austin
!
! ================================ V E R S I O N ==================================================
! V0.00: 04/23/2018 - File initiated.
! V1.00: 04/23/2018 - Initiated: Compiled without error for the first time.
!
! File version $Id $
!
! Last update: 04/23/2018
!
! ================================ S U B R O U T I N E ============================================
!
! ================================ F U N C T I O N ================================================
!
! ================================ L O C A L   V A R I A B L E S ==================================
! (Refer to the main code to see the list of imported variables)
!  . . . . . . . . . . . . . . . . Variables . . . . . . . . . . . . . . . . . . . . . . . . . . .
!
!##################################################################################################

module Timer_mod


! Libraries =======================================================================================

! User defined modules ============================================================================
use Parameters_mod

implicit none

! Time signature of the model
type, public:: TimeDate_tp
  integer(kind=Smll)  :: Year, Month, Day                ! Date variables
  integer(kind=Smll)  :: Hour, Minute, Seconds, S100th   ! Time variables
end type TimeDate_tp

! Holds data about the run time
type, public :: Timer_tp
  logical,        private:: started    = .FALSE.
  logical,        private:: stopped    = .FALSE.
  real(kind=dbl), private:: startTime  = 0.0_dbl
  real(kind=dbl), private:: finishTime = 0.0_dbl

  integer(kind=Lng) :: startSys, endSys, clock_rate

  contains
    procedure, pass :: start => startTimer
    procedure, pass :: stop  => stopTimer
    procedure, pass :: elapsedTime

end type Timer_tp

private
INTEGER, PARAMETER, PUBLIC  :: TC_SECONDS = 0, TC_MINUTES = 1, TC_HOURS = 2

contains

!##################################################################################################
! Purpose: This subroutine records the initial/begin time.
!
! Developed by: Babak Poursartip
!
! The Institute for Computational Engineering and Sciences (ICES)
! The University of Texas at Austin
!
! ================================ V E R S I O N ==================================================
! V0.00: 04/23/2018 - Subroutine initiated.
! V1.00: 04/23/2018 - Initiated: Compiled without error for the first time.
!
! File version $Id $
!
! Last update: 04/23/2018
!
! ================================ L O C A L   V A R I A B L E S ==================================
! (Refer to the main code to see the list of imported variables)
!  . . . . . . . . . . . . . . . . Variables . . . . . . . . . . . . . . . . . . . . . . . . . . .
!
!##################################################################################################

subroutine startTimer(this)

implicit none
class(Timer_tp) :: this

this%started = .TRUE.
call CPU_TIME(this%startTime)

end subroutine startTimer


!##################################################################################################
! Purpose: This subroutine records the end time.
!
! Developed by: Babak Poursartip
!
! The Institute for Computational Engineering and Sciences (ICES)
! The University of Texas at Austin
!
! ================================ V E R S I O N ==================================================
! V0.00: 04/23/2018 - Subroutine initiated.
! V1.00: 04/23/2018 - Initiated: Compiled without error for the first time.
!
! File version $Id $
!
! Last update: 04/23/2018
!
! ================================ L O C A L   V A R I A B L E S ==================================
! (Refer to the main code to see the list of imported variables)
!  . . . . . . . . . . . . . . . . Variables . . . . . . . . . . . . . . . . . . . . . . . . . . .
!
!##################################################################################################

subroutine stopTimer(this)

implicit none
class(Timer_tp) :: this
call CPU_TIME(this%finishTime)
this%stopped = .TRUE.

end subroutine stopTimer


!##################################################################################################
! Purpose: This subroutine computes the elapsed time.
!
! Developed by: Babak Poursartip
!
! The Institute for Computational Engineering and Sciences (ICES)
! The University of Texas at Austin
!
! ================================ V E R S I O N ==================================================
! V0.00: 04/23/2018 - Subroutine initiated.
! V1.00: 04/23/2018 - Initiated: Compiled without error for the first time.
!
! File version $Id $
!
! Last update: 04/23/2018
!
! ================================ L O C A L   V A R I A B L E S ==================================
! (Refer to the main code to see the list of imported variables)
!  . . . . . . . . . . . . . . . . Variables . . . . . . . . . . . . . . . . . . . . . . . . . . .
!
!##################################################################################################

function elapsedTime(this,units)

implicit none

class(Timer_tp)   :: this
integer, optional :: units
real(kind=dbl)    :: elapsedTime

if ( .NOT. this%started ) then
  elapsedTime = 0.0_dbl
  return
end if


if ( .NOT. this%stopped ) then
  call this%stop()
end if

elapsedTime = this%finishTime - this%startTime

if ( present(units) ) then
  select case ( units )
     case (TC_MINUTES)

        elapsedTime = elapsedTime/60.0_dbl

     case (TC_HOURS)

        elapsedTime = elapsedTime/3600.0_dbl

     case default

  end select
end if

end function elapsedTime

end module Timer_mod
