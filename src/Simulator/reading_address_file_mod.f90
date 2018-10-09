
!##################################################################################################
! Purpose: Reads file name and directories from the address file.
!
! Developed by:  Babak Poursartip
! Supervised by: Clint Dawson
!
! The Institute for Computational Engineering and Sciences (ICES)
! The University of Texas at Austin
!
! ================================ V E R S I O N ==================================================
! V0.10: 02/22/2018 - Initiation.
! V0.10: 03/08/2018 - Initiated: Compiled without error.
! V1.00: 04/20/2018 - Major modifications
! V2.00: 05/15/2018 - Separating model from the input
! V2.01: 07/26/2018 - Modifications on the style - submodule
!
! File version $Id $
!
! Last update: 08/09/2018
!
! ================================ S U B R O U T I N E ============================================
! Input_Address_sub: Reads file name and directories from the address file.
! Input_Analysis_sub
! Python_Visualizer_sub
!
! ================================ F U N C T I O N ================================================
!
! ================================ L O C A L   V A R I A B L E S ==================================
! (Refer to the main code to see the list of imported variables)
!  . . . . . . . . . . . . . . . . Variables . . . . . . . . . . . . . . . . . . . . . . . . . . .
!
!##################################################################################################

module Input_mod

use Parameters_mod
use messages_and_errors_mod

implicit none
private

! Holds info. (name, dir. output dir.) of the model, initialized by subroutine Input_Address_sub.
type Input_Data_tp
  character (kind = 1, Len = 150) :: ModelName     ! Name of the model input file
  character (kind = 1, Len = 150) :: ModelNameParallel ! Name of the model input file with info
                                                       ! about Parallel simulation
  character (kind = 1, Len = 150) :: InputDir      ! Directory of the input file.
  character (kind = 1, Len = 150) :: AnalysisDir   ! Directory of Analysis input file.
  character (kind = 1, Len = 150) :: OutputDir     ! Directory of output files (Results)
  character (kind = 1, Len = 150) :: AnalysisOutputDir ! Dir of output file for each analysis
  character (kind = 1, Len = 150) :: VisualizerDir ! Dir of output file for visualizer(python)

  ! Holds the names of the analysis input files
  character (kind = 1, Len = 150), dimension(:), allocatable :: AnalysesNames

  ! rank and size of MPI
  Character(kind = 1, len = 20) :: IndexRank !Rank no in the Char. fmt to add to input file Name
  Character(kind = 1, len = 20) :: IndexSize !Size no in the Char. fmt to add to input file Name

  !integer(kind=Smll):: OutputType                ! Output Type: 1: ordinary-2: HDF5
  integer(kind=Smll) :: NumberOfAnalyses          ! Number of analysis
  integer            :: size, rank                ! Size and rank of Parallel MPI

  real(kind=SGL) :: Version               ! Holds the version of the code.

  contains
    procedure :: Input      => Input_Address_sub
    procedure :: visualizer => Python_Visualizer_sub
end type Input_Data_tp

! Contains all information about the domain, required
type AnalysisData_tp
  integer(kind=Lng), len :: TotalNNodes
  integer(kind=Lng), len :: TotalNReaches

  integer(kind=Smll) :: AnalysisType=2_smll! Analysis Type -1:1D Lax-Wendroff
                                           !               -2:1D Lax-Wendroff with limiter
  integer(kind=Smll) :: limiter  = 1_smll  ! limiter type
  integer(kind=Smll) :: Junction_Model ! 1 energy based junction method
                                       ! 2 momentum based junction method

  integer(kind=Lng)  :: Plot_Inc = 500_Lng ! Increment to record the results for visualization

  real(kind=DBL) :: TotalTime    = 0.0_dbl ! Total simulation time (in seconds)
  real(kind=DBL) :: TimeStep     = 0.0_dbl ! Time Step

  real(kind=DBL) :: h_dw         = 0.0_dbl ! Downstream water depth (in meters)

  contains
    procedure Analysis => Input_Analysis_sub
end type AnalysisData_tp

public:: AnalysisData_tp, Input_Data_tp

interface

!##################################################################################################
! Purpose: This subroutine/function reads the general information about the name of the simulation
!           model, directories, etc.
!
! Developed by: Babak Poursartip
! Supervised by:
!
! The Institute for Computational Engineering and Sciences (ICES)
! The University of Texas at Austin
!
! ================================ V E R S I O N ==================================================
! V0.1: 02/21/2018 - Initiation.
! V1.0: 03/01/2018 - Compiled without error.
! V1.1: 04/10/2018 - Minor modifications.
! V1.2: 07/26/2018 - Modifications on the style - submodule
!
! File version $Id $
!
! Last update: 07/26/2018
!
! ================================ L O C A L   V A R I A B L E S ==================================
! (Refer to the main code to see the list of imported variables)
!  . . . . . . . . . . . . . . . . Variables . . . . . . . . . . . . . . . . . . . . . . . . . . .
!
!##################################################################################################

module subroutine Input_Address_sub(this)

! User defined modules ============================================================================
Implicit None

! Global Variables ================================================================================
! - Types -----------------------------------------------------------------------------------------
class(Input_Data_tp), intent(out) :: this  ! Holds info. (name, dir, output dir) of the model

end subroutine Input_Address_sub


!##################################################################################################
! Purpose: This subroutine reads information required for a particular analysis.
!
! Developed by: Babak Poursartip
! Supervised by: Clint Dawson
!
! The Institute for Computational Engineering and Sciences (ICES)
! The University of Texas at Austin
!
! ================================ V E R S I O N ==================================================
! V0.10: 02/13/2018 - Initiation.
! V1.00: 03/01/2018 - Compiled with no errors/warnings for the first time.
! V1.00: 04/10/2018 - Minor modifications in the classes/objects.
! V1.10: 07/26/2018 - Modifications on the style - submodule
!
! File version $Id $
!
! Last update: 07/26/2018
!
! ================================ L O C A L   V A R I A B L E S ==================================
! (Refer to the main code to see the list of imported variables)
!  . . . . . . . . . . . . . . . . Variables . . . . . . . . . . . . . . . . . . . . . . . . . . .
!
!##################################################################################################

module subroutine Input_Analysis_sub(this, i_analyses, ModelInfo)

Implicit None

! Global Variables ================================================================================

! - integer Variables -----------------------------------------------------------------------------
integer(kind=Smll), intent(In) :: i_analyses

! - Types -----------------------------------------------------------------------------------------
class(AnalysisData_tp), intent(inout) :: this      ! Holds analysis information
type(Input_Data_tp), intent(inout) :: ModelInfo  ! Holds info. (name, dir, output dir) of the model

end subroutine Input_Analysis_sub

!##################################################################################################
! Purpose: This subroutine provides the necessary information for the python script to visualize
!          the output of the parallel code.
!
! Developed by: Babak Poursartip
! Supervised by: Clint Dawson
!
! The Institute for Computational Engineering and Sciences (ICES)
! The University of Texas at Austin
!
! ================================ V E R S I O N ==================================================
! V0.00: 05/31/2018 - File initiated.
! V1.00: 05/31/2018 - File initiated.
! V1.10: 07/26/2018 - Modifications on the style - submodule
!
! File version $Id $
!
! Last update: 07/26/2018
!
! ================================ L O C A L   V A R I A B L E S ==================================
! (Refer to the main code to see the list of imported variables)
!  . . . . . . . . . . . . . . . . Variables . . . . . . . . . . . . . . . . . . . . . . . . . . .
!
!##################################################################################################

module subroutine Python_Visualizer_sub(this, AnalysisInfo, i_analyses)

implicit none

! Global variables ================================================================================

integer(kind=Smll) :: i_analyses     ! loop index to read the analyses files

! - types -----------------------------------------------------------------------------------------
class(Input_Data_tp) :: this
type(AnalysisData_tp) :: AnalysisInfo

end subroutine Python_Visualizer_sub

end interface

end module Input_mod
