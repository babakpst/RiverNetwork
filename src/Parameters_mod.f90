
!##################################################################################################
! Purpose: This module contains all defined Parameters.
!
! Developed by: Babak Poursartip
! Supervised by: Clint Dawson
!
! The Institute for Computational Engineering and Sciences (ICES)
! The University of Texas at Austin
!
! ================================ V E R S I O N ==================================================
! V0.1: 08/02/2018 - Initiation.
!
! File version $Id
!
! Last update: 08/02/2018
!
! ================================ L O C A L   V A R I A B L E S ==================================
! (Refer to the main code to see the list of imported variables)
!  . . . . . . . . . . . . . . . . Variables . . . . . . . . . . . . . . . . . . . . . . . . . . .
!
!##################################################################################################

Module Parameters_mod

Implicit None


! Define types of integer AND Real variables ======================================================
integer(2), Parameter, Public :: SGL =SELECTED_Real_kind(P=6, R=37 )  ! EQUIVALENT TO Real (4)
integer(2), Parameter, Public :: DBL =SELECTED_Real_kind(P=13, R=200) ! EQUIVALENT TO Real (8)

integer(2), Parameter, Public :: Tiny=SELECTED_INT_kind(1 )           ! EQUIVALENT TO integer(1)
integer(2), Parameter, Public :: Smll=SELECTED_INT_kind(3 )           ! EQUIVALENT TO integer(2)
integer(2), Parameter, Public :: Shrt=SELECTED_INT_kind(8 )           ! EQUIVALENT TO integer(4)
integer(2), Parameter, Public :: Lng =SELECTED_INT_kind(10)           ! EQUIVALENT TO integer(8)

! MATHEMATICAL CONSTATNS ==========================================================================
!#integer(kind=Shrt), Parameter, Public  ::
real(kind=DBL), Parameter, Public  ::  PI=3.141592653589793238_DBL

! FORMATS =========================================================================================
character(87),  Parameter, Public :: Fmt_DATE="(' DATE :  ',I2.2,' - ',I2.2,' - ',I4,/,' TIME : ',I2.2,':',I2.2,':',I2.2,':',I2.2,/ )"
character(27),  Parameter, Public :: Fmt_End="('PRESS ENTER TO End ...')"
character(80),  Parameter, Public :: Fmt_ERR1_OPEN="( 'ERROR IN OPEN STATEMENT. Unit NUMBER=', I3, '   ERROR NUMBER IS=', I4  )"
character(91),  Parameter, Public :: Fmt_ERR2_OPEN="('End-OF-FILE ERROR IN OPEN STATEMENT. Unit NUMBER=', I3, '   ERROR NUMBER IS=', I4  )"
character(81),  Parameter, Public :: Fmt_ERR1_Close="( 'ERROR IN Close STATEMENT. Unit NUMBER=', I3, '   ERROR NUMBER IS=', I4  )"
character(163), Parameter, Public :: Fmt_NM="(' FILE Name : ', A20,//,' Directories :',/, 'INPUT FILE DIRECTORY     : ', A100,/, 'OUTPUT FILES DIRECTORY   : ', A100 )"
character(41),  Parameter, Public :: Fmt_SUC="('CONGRATULATIONS! DONE SUCCESSFULLY. ')"
character(39),  Parameter, Public :: Fmt_FL="('OOPS!!!  FAIL TO OPERATE PROPERLY.')"
character(78),  Parameter, Public :: Fmt_ALLCT="('ERROR IN ALLOCATING Arrays. ERROR NUMBER IS :', I4, '   LOCATION: ??????.')"
character(80),  Parameter, Public :: Fmt_DEALLCT="('ERROR IN DEALLOCATING Arrays. ERROR NUMBER IS :', I4, '   LOCATION: ??????.')"
character(23),  Parameter, Public :: Fmt_RUNTIME="(A,F50.2,'   SECONDS')"
character(70),  Parameter, Public :: Fmt_READ1="('ERROR IN READ STATEMENT. Unit IS : ',I5,' ERROR NUMBER IS : ', I5 )"
character(76),  Parameter, Public :: Fmt_READ2="('End OF FILE IN READ STATEMENT. Unit IS : ',I5,' ERROR NUMBER IS : ', I5 )"
character(78),  Parameter, Public :: Fmt_READ3="('End OF RECORD IN READ STATEMENT. Unit IS : ',I5,' ERROR NUMBER IS : ', I5 )"
character(71),  Parameter, Public :: Fmt_write1="('ERROR IN write STATEMENT. Unit IS : ',I5,' ERROR NUMBER IS : ', I5 )"
character(143), Parameter, Public :: Fmt_Element1="('Error in the element type. Either there is a mistake in the input file for element type or element type in not available in the code yet.')"
character(144), Parameter, Public :: Fmt_Element2="('Error in the element type. This element number',I3,'is not available in the list of this code. Check the input file for element number',I19)"

! Unit NUMBERS OF EXTERNAL FILES ==================================================================
! Address file
integer(kind=Smll), Parameter, Public :: FileAdr=500 ! Address file that holds the model name & directories (.txt)

! Input files
integer(kind=Smll), Parameter, Public :: FileDataModel=501 ! Input file (.dataModel)
integer(kind=Smll), Parameter, Public :: UnInptXYZ=502 ! Input file for node coordinates (.XYZ)
integer(kind=Smll), Parameter, Public :: UnInptCnn=503 ! Input file for connectivities of elements (.Cnn)
integer(kind=Smll), Parameter, Public :: UnInptCnt=504 ! Input file for node constraints (.Cnt)
integer(kind=Smll), PARAMETER, PUBLIC :: UnInptAna=510 ! input file for analysis (.data)
integer(kind=Smll), PARAMETER, PUBLIC :: UnInptMat=511 ! input file for material property (.Mat)

! Debugging files
integer(kind=Smll), Parameter, Public  :: Un_CHK=599 ! Scratch file for debugging (.Chk)

!Output files
integer(kind=Smll), Parameter, Public  :: FileInfo=600 ! Model information file (.Inf)


! Analysis case number ============================================================================
integer(kind=Smll), Parameter, Public :: AnalysisType_1D=100 ! 1D Shallow water simulation


! Element Types Number ============================================================================
! Solid Element
!integer(kind=Tiny), Parameter, Public  :: El2d4NSldPS  =1 ! Element: 2D-4 noded-Solid

! PML Elements

! Spectral Element


! User defined types ==============================================================================
! Time signature of the model
type TimeDate_tp
  integer(kind=Smll)  :: Year, Month, Day                ! Date variables
  integer(kind=Smll)  :: Hour, Minute, Seconds, S100th   ! Time variables
end type TimeDate_tp

! Holds info. (name, Dir, output directory) of the model
type Input_Data_tp
  character (kind = 1, Len = 30 ) :: ModelName     ! Name of the model input file
  character (kind = 1, Len = 150) :: InputDir      ! Directory of the input file.
  character (kind = 1, Len = 150) :: AnalysisDir   ! Directory of Analysis input file.
  character (kind = 1, Len = 150) :: OutputDir     ! Directory of output files (Results)
  character (kind = 1, Len = 150) :: AnalysisOutputDir! Directory of output file for each analysis
  character (kind = 1, Len = 150), dimension(:), allocatable :: AnalysesNames! Holds the name of the analysis input file

  integer(kind=Smll) :: AnalysisType      ! Analysis Type: 1: 1D-2: 2D
  !integer(kind=Smll):: OutputType        ! Output Type: 1: ordinary-2: HDF5
  integer(kind=Smll) :: NumberOfAnalyses  ! Number of analysis

  real(kind=SGL) :: Version               ! Holds the version of the code.
end type Input_Data_tp


! Holds the command argument
type ArgCommands
  integer(kind=Smll) :: ArgCount      ! Counts number of argument
  integer(kind=Shrt), allocatable, dimension(:) :: Argstatus     ! Counts number of argument
  integer(kind=Shrt), allocatable, dimension(:) :: Length          ! Holds the length of each arg
  character (kind = 1, Len = 50), allocatable, dimension(:) :: Arg  ! Holds the entered argument
end type

type timing
  real(kind=DBL):: Time_Start, Time_End !TIME Variables for total run time
  real(kind=DBL):: Input_Starts, Input_Ends ! required time to read the input file
end type timing

type InitialData_tp
  integer(kind=Lng):: NoReaches ! Number of reaches
  real(kind=DBL):: TotalTime ! Total simulation time (in seconds)
  real(kind=DBL):: TimeStep  ! Time Step
  real(kind=DBL):: Q_Up      ! Upstream boundary condition, constant flow (m^3/s)
  real(kind=DBL):: h_dw      ! Downstream water depth (in meters)
  real(kind=DBL):: CntrlV    ! Initial control volume
  real(kind=DBL):: CntrlV_ratio  ! Initial control volume ration, used to initialize data
end type InitialData_tp

type geometry_tp
  real(kind=DBL), allocatable, dimension(:) :: ReachLength ! Stores the length of each reach
  real(kind=DBL), allocatable, dimension(:) :: ReachDisc  ! Stores the no. of control volume in each reach
  real(kind=DBL), allocatable, dimension(:) :: ReachType  ! Stores reach type
  real(kind=DBL), allocatable, dimension(:) :: ReachSlope  ! Stores the slope of each reach
  real(kind=DBL), allocatable, dimension(:) :: ReachManning ! Stores the Manning's number for each reach
  real(kind=DBL), allocatable, dimension(:) :: ReachWidth ! Stores the width of each reach
end type geometry_tp



Contains


End Module Parameters_mod

