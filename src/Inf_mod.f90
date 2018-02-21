
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
!
! File version $Id $
!
! Last update: 12/02/2018
!
! ================================ S U B R O U T I N E ============================================
!
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

Module Information

use Parameters

Implicit None

  Interface Info
    Module Procedure InfBasic, InfTime
  End Interface  Info


Contains


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
! Last update: 12/02/2018
!
! ================================ L O C A L   V A R I A B L E S ==================================
! (Refer to the main code to see the list of imported variables)
!  . . . . . . . . . . . . . . . . Variables . . . . . . . . . . . . . . . . . . . . . . . . . . .
!
!##################################################################################################

Subroutine InfBasic ( &
!                                               & ! Integer Variables
!                                               & ! Real Variables
!                                               & ! Integer Arrays
!                                               & ! Real Arrays
!                                               & ! Characters
TimeDate, ModelInfo                             & ! Types
)

Implicit None

! =========================== Global Variables ====================================================

! - Integer Variables -----------------------------------------------------------------------------
!Integer (Kind=Smll), Intent(In)     ::
!#Integer (Kind=Shrt), Intent(InOut) ::
!#Integer (Kind=Shrt), Intent(OUT)   ::
! - Real Variables --------------------------------------------------------------------------------
!#Real (Kind=DBL), Intent(In)    ::
!#Real (Kind=DBL), Intent(InOut) ::
!#Real (Kind=DBL), Intent(OUT)   ::
! - complex Variables -----------------------------------------------------------------------------
!#complex, Intent(In)    ::
!#complex, Intent(InOut) ::
!#complex, Intent(OUT)   ::
! - Integer Arrays --------------------------------------------------------------------------------
!#Integer (Kind=Shrt), Intent(In), Dimension (:  )  ::
!#Integer (Kind=Shrt), Intent(In), Dimension (:,:)  ::
!#Integer (Kind=Shrt), Intent(In)    ::
!#Integer (Kind=Shrt), Intent(InOut) ::
!#Integer (Kind=Shrt), Intent(OUT)   ::
! - Real Arrays -----------------------------------------------------------------------------------
!#Real (Kind=DBL), Intent(In)    ::
!#Real (Kind=DBL), Intent(InOut) ::
!#Real (Kind=DBL), Intent(OUT)   ::
! - complex Variables -----------------------------------------------------------------------------
!#complex, Intent(In)    ::
!#complex, Intent(InOut) ::
!#complex, Intent(OUT)   ::
! - Character Variables ---------------------------------------------------------------------------
!Character (Kind = 1, Len = 30 ), Intent(In) :: Name        ! Name of the Input file


! - Types -----------------------------------------------------------------------------------------
type(TimeDate_tp)    :: TimeDate   ! Indicates the time and date of simulation
type(Input_Data_tp)  :: ModelInfo  ! Holds info. (name, dir, output dir) of the model

! - Logical Variables -----------------------------------------------------------------------------
!#Logical   ::

! =========================== LOCAL Variables =====================================================
! - Integer Variables -----------------------------------------------------------------------------
!#Integer (Kind=Shrt)  ::
! - Real Variables --------------------------------------------------------------------------------
!#Real (Kind=DBL)  ::
! - complex Variables -----------------------------------------------------------------------------
!#complex  ::
! - Integer Arrays --------------------------------------------------------------------------------
!#Integer (Kind=Shrt)  ::
! - Real Arrays -----------------------------------------------------------------------------------
!#Real (Kind=DBL)  ::
! - complex Variables -----------------------------------------------------------------------------
!#complex  ::
! - Character Variables ---------------------------------------------------------------------------
!#Character   ::
! - Logical Variables -----------------------------------------------------------------------------
!#Logical   ::
! - Type DECLERATIONS -----------------------------------------------------------------------------
!#type() ::

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
! V0.1: 02/12/2018 - Initiation.
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

Subroutine InfTime(                                          &
!                                                            & ! Integer Variables
TimeE, TimeS, TimeInputE, TimeInputS, TimeSolveE, TimeSolveS & ! Real Variables
!                                                            & ! Integer Arrays
!                                                            & ! Real Arrays
!                                                            & ! Characters
!                                                            & ! Type
)

Implicit None

! =========================== Global Variables ====================================================

! - Integer Variables -----------------------------------------------------------------------------
!Integer (Kind=Lng ), Intent(In) ::

! - Real Variables --------------------------------------------------------------------------------
Real (Kind=Dbl), Intent(In)    :: TimeE, TimeS, TimeInputE, TimeInputS, TimeSolveE, TimeSolveS

! - complex Variables -----------------------------------------------------------------------------
!#complex, Intent(In)    ::
!#complex, Intent(InOut) ::
!#complex, Intent(OUT)   ::
! - Integer Arrays --------------------------------------------------------------------------------
!#Integer (Kind=Shrt), Intent(In), Dimension (:  )  ::
!#Integer (Kind=Shrt), Intent(In), Dimension (:,:)  ::
!#Integer (Kind=Shrt), Intent(In)    ::
!#Integer (Kind=Shrt), Intent(InOut) ::
!#Integer (Kind=Shrt), Intent(OUT)   ::
! - Real Arrays -----------------------------------------------------------------------------------
!#Real (Kind=Dbl), Intent(In), Dimension (:  )  ::
!#Real (Kind=Dbl), Intent(InOut), Dimension (:  )  ::
!#Real (Kind=Dbl), Intent(OUT), Dimension (:  )  ::
! - complex Variables -----------------------------------------------------------------------------
!#complex, Intent(In)    ::
!#complex, Intent(InOut) ::
!#complex, Intent(OUT)   ::
! - Character Variables ---------------------------------------------------------------------------
!#Character   ::
! - Logical Variables -----------------------------------------------------------------------------
!#Logical   ::
! - Type DECLERATIONS -----------------------------------------------------------------------------
!#type() ::
! =========================== Local Variables =====================================================
! - Integer Variables -----------------------------------------------------------------------------
!#Integer (Kind=Shrt)  ::
! - Real Variables --------------------------------------------------------------------------------
!#Real (Kind=Dbl)  ::
! - complex Variables -----------------------------------------------------------------------------
!#complex  ::
! - Integer Arrays --------------------------------------------------------------------------------
!#Integer (Kind=Shrt)  ::
! - Real Arrays -----------------------------------------------------------------------------------
!#Real (Kind=Dbl)  ::
! - complex Variables -----------------------------------------------------------------------------
!#complex  ::
! - Character Variables ---------------------------------------------------------------------------
!#Character   ::
! - Logical Variables -----------------------------------------------------------------------------
!#Logical   ::

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

End Module Information
