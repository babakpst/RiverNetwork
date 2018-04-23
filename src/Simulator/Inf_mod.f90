
!##################################################################################################
! Purpose: This module writes down information about the simulation in the info.txt file.
!
! Developed by: Babak Poursartip
! Supervised by: Clint Dawson
!
! The Institute for Computational Engineering and Sciences (ICES)
! The University of Texas at Austin
!
! ================================ V E R S I O N ==================================================
! V0.1: 02/12/2018 - Initiation.
! V0.2: 02/23/2018 - Adding the header subroutine.
!
! File version $Id $
!
! Last update: 02/23/2018
!
! ================================ S U B R O U T I N E ============================================
!
! Header: Writes down the introduction of the code.
! InfBasic: writes the initial information in the info file.
! InfTime:  writes the simulation time.
!
! ================================ F U N C T I O N ================================================
!
! ================================ L O C A L   V A R I A B L E S ==================================
! (Refer to the main code to see the list of imported variables)
!  . . . . . . . . . . . . . . . . Variables . . . . . . . . . . . . . . . . . . . . . . . . . . .
!
!
!##################################################################################################

Module Information_mod

use Timer_mod
use Parameters_mod
use Input_mod


Implicit None

  Interface Info
    Module Procedure InfBasic, InfTime, Header
  End Interface  Info


Contains


!##################################################################################################
! Purpose: This subroutine writes down the header on screen.
!
! Developed by: Babak Poursartip
! Supervised by: Clint Dawson
!
! The Institute for Computational Engineering and Sciences (ICES)
! The University of Texas at Austin
!
! ================================ V E R S I O N ==================================================
! V0.1: 02/23/2018 - Initiation.
!
! File version $Id $
!
! Last update: 02/23/2018
!
! ================================ L O C A L   V A R I A B L E S ==================================
! (Refer to the main code to see the list of imported variables)
!  . . . . . . . . . . . . . . . . Variables . . . . . . . . . . . . . . . . . . . . . . . . . . .
!
!##################################################################################################

subroutine Header(version)

implicit none

real(kind=SGL):: version


! write some inFormation on screen
write(*,*)
write(*,*)
write(*,*)"=========================================================="
write(*,*)"=========================================================="
write(*,*)"     Numerical simulation of 2D Shallow water Equation"
write(*,*)"             Developer: Babak Poursartip"
write(*,*)"             Supervised by: Clint Dawson"
write(*,*)
write(*,*)"    Institute for Computational Engineering and Sciences"
write(*,*)
write(*,fmt="(A,F6.2)") "                   Version:", version
write(*,*)
write(*,*)"=========================================================="
write(*,*)"=========================================================="
write(*,*)

end subroutine Header


!##################################################################################################
! Purpose: This subroutine writes down the initial data about the simulation.
!
! Developed by: Babak Poursartip
! Supervised by: Clint Dawson
!
! The Institute for Computational Engineering and Sciences (ICES)
! The University of Texas at Austin
!
! ================================ V E R S I O N ==================================================
! V0.1: 02/12/2018 - Initiation.
!
! File version $Id $
!
! Last update: 02/23/2018
!
! ================================ L O C A L   V A R I A B L E S ==================================
! (Refer to the main code to see the list of imported variables)
!  . . . . . . . . . . . . . . . . Variables . . . . . . . . . . . . . . . . . . . . . . . . . . .
!
!##################################################################################################

Subroutine InfBasic (TimeDate, ModelInfo)

Implicit None

! =========================== Global Variables ====================================================

! - Types -----------------------------------------------------------------------------------------
type(TimeDate_tp)    :: TimeDate   ! Indicates the time and date of simulation
type(Input_Data_tp)  :: ModelInfo  ! Holds info. (name, dir, output dir) of the model

! - Logical Variables -----------------------------------------------------------------------------
!#Logical   ::

! =========================== LOCAL Variables =====================================================

! CODE ============================================================================================

! write INFORMATION
write(FileInfo, Fmt_DATE) TimeDate%Month, TimeDate%Day,    TimeDate%Year, &
                        TimeDate%Hour,  TimeDate%Minute, TimeDate%Seconds, TimeDate%S100th
write(FileInfo, Fmt_NM)ModelInfo%ModelName, ModelInfo%InputDir, ModelInfo%OutputDir

write (FileInfo,*)" Analysis Type "

write(*, *) 'End Subroutine < InfBasic >'
Return
End Subroutine InfBasic


!##################################################################################################
! Purpose: This subroutine writes down the initial data about the simulation.
!
! Developed by: Babak Poursartip
! Supervised by: Clint Dawson
!
! The Institute for Computational Engineering and Sciences (ICES)
! The University of Texas at Austin
!
! ================================ V E R S I O N ==================================================
! V0.01: 02/12/2018 - Initiation.
! V0.10: 03/08/2018 - Initiated: Compiled without error.
!
! File version $Id $
!
! Last update: 12/02/2018
!
! ================================ L O C A L   V A R I A B L E S ==================================
! (Refer to the main code to see the list of imported variables)
!  . . . . . . . . . . . . . . . . Variables . . . . . . . . . . . . . . . . . . . . . . . . . . .
!
!##################################################################################################

Subroutine InfTime(TimeE, TimeS, TimeInputE, TimeInputS, TimeSolveE, TimeSolveS)

Implicit None

! =========================== Global Variables ====================================================

! - Real Variables --------------------------------------------------------------------------------
Real (kind=Dbl), Intent(In)    :: TimeE, TimeS, TimeInputE, TimeInputS, TimeSolveE, TimeSolveS

! =========================== Local Variables =====================================================

! =========================== Subroutine CODE =====================================================

write(FileInfo,*)

!write(*     ,Fmt_RUNTIME) "TOTAL"   , TimeE - TimeS

write(FileInfo,*)"---------- RUNNING TIME STATISTICS ----------"

write(FileInfo,Fmt_RUNTIME) "Reading Input files           ", TimeInputE  - TimeInputS
write(FileInfo,Fmt_RUNTIME) "SOLVE                         ", TimeSolveE  - TimeSolveS
write(FileInfo,Fmt_RUNTIME) "TOTAL                         ", TimeE       - TimeS
write(FileInfo,*)

write(*     ,*) 'End Subroutine < InfTime >'


Return
End Subroutine InfTime

End Module Information_mod
