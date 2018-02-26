
!##################################################################################################
! Purpose: This module discretizes the domain.
!
! Developed by: Babak Poursartip
! Supervised by: Clint Dawson
!
! The Institute for Computational Engineering and Sciences (ICES)
! The University of Texas at Austin
!
! ================================ V E R S I O N ==================================================
! V0.00: 02/26/2018 - Start the module.
! V0.01: 02/26/2018 - Initiation: Compiled without error for the first time.
!
! File version $Id $
!
! Last update: 02/26/2018
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

Module Discretization_mod

! Libraries =======================================================================================

! User defined modules ============================================================================
use Parameters_mod

Implicit None

Public
Private

  Interface Discretize
    Module Procedure Discretize_1D
  End Interface


Contains





End Module Discretization_mod
