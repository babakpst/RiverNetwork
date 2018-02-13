

!##################################################################################################
! Purpose: This module reads all data for simulation.
!
! Developed by: Babak Poursartip
! Supervised by: Clint Dawson
!
! The Institute for Computational Engineering and Sciences (ICES)
! The University of Texas at Austin
!
! ================================ V E R S I O N ==================================================
! V0.1: 02/13/2018 - Initiation.
!
! File version $Id $
!
! Last update: 13/02/2018
!
! ================================ S U B R O U T I N E ============================================
!
!
! ================================ F U N C T I O N ================================================
!
! ================================ L O C A L   V A R I A B L E S ==================================
! (Refer to the main code to see the list of imported variables)
!  . . . . . . . . . . . . . . . . Variables . . . . . . . . . . . . . . . . . . . . . . . . . . .
!
!##################################################################################################

module Input_mod

use Parameters

implicit none


interface Input
  module procedure Input_Basic, Input_Array, Input_Analysis
end interface Input

contains

!##################################################################################################
! Purpose: This subroutine reads the initial information for simulation, so that the arrays can be
!          allocated.
!
! Developed by: Babak Poursartip
! Supervised by: Clint Dawson
!
! The Institute for Computational Engineering and Sciences (ICES)
! The University of Texas at Austin
!
! ================================ V E R S I O N ==================================================
! V0.1: 02/13/2018 - Initiation.
!
! File version $Id $
!
! Last update: 02/13/2018
!
! ================================ L O C A L   V A R I A B L E S ==================================
! (Refer to the main code to see the list of imported variables)
!  . . . . . . . . . . . . . . . . Variables . . . . . . . . . . . . . . . . . . . . . . . . . . .
!
!##################################################################################################

Subroutine Input_Basic(                                               &
!                                                                     & ! Integer (1) Variables
!                                                                     & ! Integer (2) Variables
!                                                                     & ! Integer (4) Variables
!                                                                     & ! Integer (8) Variables
!                                                                     & ! Real Variables
!                                                                     & ! Integer Arrays
!                                                                     & ! Real Arrays
!                                                                     & ! Characters
!                                                                     & ! Type
)

Implicit None ;

! Global Variables ================================================================================

! - Integer Variables -----------------------------------------------------------------------------
!#Integer (Kind=Shrt), Intent(In)    ::
!#Integer (Kind=Shrt), Intent(InOut) ::
!#Integer (Kind=Shrt), Intent(OUT)   ::
! - Real Variables --------------------------------------------------------------------------------
!#Real (Kind=Dbl),     Intent(In)    ::
!#Real (Kind=Dbl),     Intent(InOut) ::
!#Real (Kind=Dbl),     Intent(OUT)   ::
! - Complex Variables -----------------------------------------------------------------------------
!#Complex,             Intent(In)    ::
!#Complex,             Intent(InOut) ::
!#Complex,             Intent(OUT)   ::
! - Integer Arrays --------------------------------------------------------------------------------
!#Integer (Kind=Shrt), Intent(In),    Dimension (:  )  ::
!#Integer (Kind=Shrt), Intent(In),    Dimension (:,:)  ::
!#Integer (Kind=Shrt), Intent(In)    ::
!#Integer (Kind=Shrt), Intent(InOut) ::
!#Integer (Kind=Shrt), Intent(OUT)   ::
! - Real Arrays -----------------------------------------------------------------------------------
!#Real (Kind=Dbl),     Intent(In),    Dimension (:  )  ::
!#Real (Kind=Dbl),     Intent(InOut), Dimension (:  )  ::
!#Real (Kind=Dbl),     Intent(OUT),   Dimension (:  )  ::
! - Character Variables ---------------------------------------------------------------------------
!#Character (Kind = ?, Len = ? ) ::
! - Logical Variables -----------------------------------------------------------------------------
!#Logical   ::
! - Types -----------------------------------------------------------------------------------------
!#Type() ::
! Local Variables =================================================================================
! - Integer Variables -----------------------------------------------------------------------------
!#Integer (Kind=Shrt)  ::
! - Real Variables --------------------------------------------------------------------------------
!#Real (Kind=Dbl)      ::
! - Complex Variables -----------------------------------------------------------------------------
!#Complex              ::
! - Integer Arrays --------------------------------------------------------------------------------
!#Integer (Kind=Shrt), Dimension (:)  ::
!#Integer (Kind=Shrt), Allocatable, Dimension (:)  ::
! - Real Arrays -----------------------------------------------------------------------------------
!#Real (Kind=Dbl), Dimension (:)      ::
!#Real (Kind=Dbl), Allocatable, Dimension (:)  ::
! - Character Variables ---------------------------------------------------------------------------
!#Character (Kind = ?, Len = ? ) ::
! - Logical Variables -----------------------------------------------------------------------------
!#Logical   ::  ;
! - Type ------------------------------------------------------------------------------------------

! CODE ============================================================================================



!#Write(*    ,*) 'End Subroutine < sub >'
!#Write(UnInf,*) 'End Subroutine < sub >'
Return ;
End Subroutine Input_Basic

!##################################################################################################
! Purpose: This subroutine reads the arrays.
!
! Developed by: Babak Poursartip
! Supervised by: Clint Dawson
!
! The Institute for Computational Engineering and Sciences (ICES)
! The University of Texas at Austin
!
! ================================ V E R S I O N ==================================================
! V0.1: 02/13/2018 - Initiation.
!
! File version $Id $
!
! Last update: 02/13/2018
!
! ================================ L O C A L   V A R I A B L E S ==================================
! (Refer to the main code to see the list of imported variables)
!  . . . . . . . . . . . . . . . . Variables . . . . . . . . . . . . . . . . . . . . . . . . . . .
!
!##################################################################################################



!##################################################################################################
! Purpose: This subroutine reads information required for a particular analysis
!
! Developed by: Babak Poursartip
! Supervised by: Clint Dawson
!
! The Institute for Computational Engineering and Sciences (ICES)
! The University of Texas at Austin
!
! ================================ V E R S I O N ==================================================
! V0.1: 02/13/2018 - Initiation.
!
! File version $Id $
!
! Last update: 02/13/2018
!
! ================================ L O C A L   V A R I A B L E S ==================================
! (Refer to the main code to see the list of imported variables)
!  . . . . . . . . . . . . . . . . Variables . . . . . . . . . . . . . . . . . . . . . . . . . . .
!
!##################################################################################################



end module Input_mod
