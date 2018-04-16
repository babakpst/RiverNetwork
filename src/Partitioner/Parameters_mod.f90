
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
! V0.00: 04/16/2018 - Initiation.
! V0.10: 03/08/2018 - Initiated: Compiled without error.
! V1.00: 03/20/2018 - Finalized: Compiled without error.
! V2.00: 04/16/2018 - Initiating the partitioner.
!
! File version $Id
!
! Last update: 04/16/2018
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
real(kind=DBL), Parameter, Public :: PI=3.141592653589793238_Dbl
real(kind=Dbl), Parameter, Public :: Gravity=9.81_Dbl  ! the ground acceleration

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
integer(kind=Smll), Parameter, Public :: FileDataGeo=502 ! Input file for node coordinates (.XYZ)
integer(kind=Smll), PARAMETER, PUBLIC :: UnInptAna=510 ! input file for analysis (.data)
integer(kind=Smll), PARAMETER, PUBLIC :: UnInptMat=511 ! input file for material property (.Mat)

! Debugging files
integer(kind=Smll), Parameter, Public  :: Un_CHK=599 ! Scratch file for debugging (.Chk)

!Output files
integer(kind=Smll), Parameter, Public  :: FileInfo=600    ! Model information file (.Inf)
integer(kind=Smll), Parameter, Public  :: FileDomain=601  ! output file containing the domain (.domain)
integer(kind=Smll), Parameter, Public  :: FileResults=602 ! output file containing results (.res)
integer(kind=Smll), Parameter, Public  :: FilePartition=603 ! partitioned data (.par)

! Analysis case number ============================================================================
integer(kind=Smll), Parameter, Public :: AnalysisType_1D=1 ! 1D Shallow water simulation Lax-wendroff
integer(kind=Smll), Parameter, Public :: AnalysisType_1D_Limiter=2 ! 1D Shallow water simulation Lax-wendroff

! User defined types ==============================================================================
! Time signature of the model
type TimeDate_tp
  integer(kind=Smll)  :: Year, Month, Day                ! Date variables
  integer(kind=Smll)  :: Hour, Minute, Seconds, S100th   ! Time variables
end type TimeDate_tp

! Holds data about the run time
type timing
  real(kind=DBL):: Time_Start, Time_End !TIME Variables for total run time
  real(kind=DBL):: Input_Starts, Input_Ends ! required time to read the input file
end type timing

! Holds the command argument
type ArgCommands
  integer(kind=Smll) :: ArgCount      ! Counts number of argument
  integer(kind=Shrt), allocatable, dimension(:) :: Argstatus     ! Counts number of argument
  integer(kind=Shrt), allocatable, dimension(:) :: Length          ! Holds the length of each arg
  character (kind = 1, Len = 50), allocatable, dimension(:) :: Arg  ! Holds the entered argument
end type

End Module Parameters_mod

