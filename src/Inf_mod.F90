
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
! V0.1: 12/02/2018 - Initiation.
!
! File version $Id $
!
! Last update: 12/02/2018
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

Module Information ;


Implicit None ;

  Interface Info ;
    Module Procedure InfBasic, InfTime ;
  End Interface  Info;


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
! V0.1: 12/02/2018 - Initiation.
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
Name, Model_InDir, OutDir, InlDir,              & ! Characters
TimeDate                                        & ! Types
) ;

Implicit None ;

! =========================== Global Variables ====================================================

! - Integer Variables -----------------------------------------------------------------------------
!Integer (Kind=Smll), Intent(In)     ::  ;
!#Integer (Kind=Shrt), Intent(InOut) ::  ;
!#Integer (Kind=Shrt), Intent(OUT)   ::  ;
! - Real Variables --------------------------------------------------------------------------------
!#Real (Kind=DBL), Intent(In)    ::  ;
!#Real (Kind=DBL), Intent(InOut) ::  ;
!#Real (Kind=DBL), Intent(OUT)   ::  ;
! - Complex Variables -----------------------------------------------------------------------------
!#Complex, Intent(In)    ::  ;
!#Complex, Intent(InOut) ::  ;
!#Complex, Intent(OUT)   ::  ;
! - Integer Arrays --------------------------------------------------------------------------------
!#Integer (Kind=Shrt), Intent(In), Dimension (:  )  ::
!#Integer (Kind=Shrt), Intent(In), Dimension (:,:)  ::
!#Integer (Kind=Shrt), Intent(In)    ::  ;
!#Integer (Kind=Shrt), Intent(InOut) ::  ;
!#Integer (Kind=Shrt), Intent(OUT)   ::  ;
! - Real Arrays -----------------------------------------------------------------------------------
!#Real (Kind=DBL), Intent(In)    ::  ;
!#Real (Kind=DBL), Intent(InOut) ::  ;
!#Real (Kind=DBL), Intent(OUT)   ::  ;
! - Complex Variables -----------------------------------------------------------------------------
!#Complex, Intent(In)    ::  ;
!#Complex, Intent(InOut) ::  ;
!#Complex, Intent(OUT)   ::  ;
! - Character Variables ---------------------------------------------------------------------------
Character (Kind = 1, Len = 30 ), Intent(In) :: Name ;       ! Name of the Input file
Character (Kind = 1, Len = 150), Intent(In) :: Model_InDir; ! Directory of the input file.
Character (Kind = 1, Len = 150), Intent(In) :: InlDir ;     ! Directory of the internal files.
Character (Kind = 1, Len = 150), Intent(In) :: OutDir ;     ! Directory of the output files-Results

! - Types -----------------------------------------------------------------------------------------
type (TimeDate_tp):: TimeDate ;

! - Logical Variables -----------------------------------------------------------------------------
!#Logical   ::  ;

! =========================== LOCAL Variables =====================================================
! - Integer Variables -----------------------------------------------------------------------------
!#Integer (Kind=Shrt)  ::  ;
! - Real Variables --------------------------------------------------------------------------------
!#Real (Kind=DBL)  ::  ;
! - Complex Variables -----------------------------------------------------------------------------
!#Complex  ::  ;
! - Integer Arrays --------------------------------------------------------------------------------
!#Integer (Kind=Shrt)  ::  ;
! - Real Arrays -----------------------------------------------------------------------------------
!#Real (Kind=DBL)  ::  ;
! - Complex Variables -----------------------------------------------------------------------------
!#Complex  ::  ;
! - Character Variables ---------------------------------------------------------------------------
!#Character   ::  ;
! - Logical Variables -----------------------------------------------------------------------------
!#Logical   ::  ;
! - Type DECLERATIONS -----------------------------------------------------------------------------
!#Type() :: ;

! CODE ============================================================================================

! Write INFORMATION
Write (UnInf, Fmt_DATE) TimeDate%Month, TimeDate%Day,    TimeDate%Year, &
                        TimeDate%Hour,  TimeDate%Minute, TimeDate%Second, TimeDate%S100th ;
Write (UnInf, Fmt_NM  ) Name, Model_InDir, OutDir, InlDir ;

Write (UnInf,*)" Analysis Type " ;

Write(*     ,*) 'End Subroutine < InfBasic >' ;
Return ;
End Subroutine InfBasic ;


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
! V0.1: 12/02/2018 - Initiation.
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

Subroutine InfTime  (                                        &
!                                                            & ! Integer Variables
TimeE, TimeS, TimeInputE, TimeInputS, TimeSolveE, TimeSolveS & ! Real Variables
!                                                            & ! Integer Arrays
!                                                            & ! Real Arrays
!                                                            & ! Characters
!                                                            & ! Type
) ;

Implicit None ;

! =========================== Global Variables ====================================================

! - Integer Variables -----------------------------------------------------------------------------
!Integer (Kind=Lng ), Intent(In) :: ;

! - Real Variables --------------------------------------------------------------------------------
Real (Kind=Dbl), Intent(In)    :: TimeE, TimeS, TimeInputE, TimeInputS, TimeSolveE, TimeSolveS ;

! - Complex Variables -----------------------------------------------------------------------------
!#Complex, Intent(In)    ::  ;
!#Complex, Intent(InOut) ::  ;
!#Complex, Intent(OUT)   ::  ;
! - Integer Arrays --------------------------------------------------------------------------------
!#Integer (Kind=Shrt), Intent(In), Dimension (:  )  :: ;
!#Integer (Kind=Shrt), Intent(In), Dimension (:,:)  :: ;
!#Integer (Kind=Shrt), Intent(In)    ::  ;
!#Integer (Kind=Shrt), Intent(InOut) ::  ;
!#Integer (Kind=Shrt), Intent(OUT)   ::  ;
! - Real Arrays -----------------------------------------------------------------------------------
!#Real (Kind=Dbl), Intent(In), Dimension (:  )  ::
!#Real (Kind=Dbl), Intent(InOut), Dimension (:  )  :: ;
!#Real (Kind=Dbl), Intent(OUT), Dimension (:  )  ::  ;
! - Complex Variables -----------------------------------------------------------------------------
!#Complex, Intent(In)    ::  ;
!#Complex, Intent(InOut) ::  ;
!#Complex, Intent(OUT)   ::  ;
! - Character Variables ---------------------------------------------------------------------------
!#Character   ::  ;
! - Logical Variables -----------------------------------------------------------------------------
!#Logical   ::  ;
! - Type DECLERATIONS -----------------------------------------------------------------------------
!#Type() :: ;
! =========================== Local Variables =====================================================
! - Integer Variables -----------------------------------------------------------------------------
!#Integer (Kind=Shrt)  ::  ;
! - Real Variables --------------------------------------------------------------------------------
!#Real (Kind=Dbl)  ::  ;
! - Complex Variables -----------------------------------------------------------------------------
!#Complex  ::  ;
! - Integer Arrays --------------------------------------------------------------------------------
!#Integer (Kind=Shrt)  ::  ;
! - Real Arrays -----------------------------------------------------------------------------------
!#Real (Kind=Dbl)  ::  ;
! - Complex Variables -----------------------------------------------------------------------------
!#Complex  ::  ;
! - Character Variables ---------------------------------------------------------------------------
!#Character   ::  ;
! - Logical Variables -----------------------------------------------------------------------------
!#Logical   ::  ;

! =========================== Subroutine CODE =====================================================

Write(UnInf,*)

!Write(*     ,Fmt_RUNTIME) "TOTAL"   , TimeE - TimeS ;

Write(UnInf,*)"---------- RUNNING TIME STATISTICS ----------" ;

Write(UnInf,Fmt_RUNTIME) "Reading Input files           ", TimeInputE  - TimeInputS ;
Write(UnInf,Fmt_RUNTIME) "SOLVE                         ", TimeSolveE  - TimeSolveS ;
Write(UnInf,Fmt_RUNTIME) "TOTAL                         ", TimeE       - TimeS ;
Write(UnInf,*)

Write(*     ,*) 'End Subroutine < InfTime >' ;


Return ;
End Subroutine InfTime ;

End Module Information ;
