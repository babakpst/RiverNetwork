
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

Module Parameters

Implicit None ;


! Define types of Integer AND Real variables ======================================================
Integer(2), Parameter, Public :: SGL =SELECTED_Real_Kind(P=6, R=37 );  ! EQUIVALENT TO Real (4)
Integer(2), Parameter, Public :: DBL =SELECTED_Real_Kind(P=13, R=200); ! EQUIVALENT TO Real (8)

Integer(2), Parameter, Public :: Tiny=SELECTED_INT_Kind(1 );           ! EQUIVALENT TO Integer (1)
Integer(2), Parameter, Public :: Smll=SELECTED_INT_Kind(3 );           ! EQUIVALENT TO Integer (2)
Integer(2), Parameter, Public :: Shrt=SELECTED_INT_Kind(8 );           ! EQUIVALENT TO Integer (4)
Integer(2), Parameter, Public :: Lng =SELECTED_INT_Kind(10);           ! EQUIVALENT TO Integer (8)

! Type DECLARATOINS ===============================================================================


! MATHEMATICAL CONSTATNS ==========================================================================
!#Integer (Kind=Shrt), Parameter, Public  :: ;
Real (Kind=DBL), Parameter, Public  ::  PI=3.141592653589793238_DBL ;

! FORMATS =========================================================================================
Character(87),  Parameter, Public :: Fmt_DATE="(' DATE :  ',I2.2,' - ',I2.2,' - ',I4,/,' TIME : ',I2.2,':',I2.2,':',I2.2,':',I2.2,/ )" ;
Character(27),  Parameter, Public :: Fmt_End="('PRESS ENTER TO End ...')" ;
Character(80),  Parameter, Public :: Fmt_ERR1_OPEN="( 'ERROR IN OPEN STATEMENT. Unit NUMBER=', I3, '   ERROR NUMBER IS=', I4  )" ;
Character(91),  Parameter, Public :: Fmt_ERR2_OPEN="('End-OF-FILE ERROR IN OPEN STATEMENT. Unit NUMBER=', I3, '   ERROR NUMBER IS=', I4  )" ;
Character(81),  Parameter, Public :: Fmt_ERR1_Close="( 'ERROR IN Close STATEMENT. Unit NUMBER=', I3, '   ERROR NUMBER IS=', I4  )" ;
Character(163), Parameter, Public :: Fmt_NM="(' FILE Name : ', A20,//,' Directories :',/, 'INPUT FILE DIRECTORY     : ', A100,/, 'OUTPUT FILES DIRECTORY   : ', A100,/, 'INTERNAL FILES DIRECTORY : ', A100,/ )" ;
Character(41),  Parameter, Public :: Fmt_SUC="('CONGRATULATIONS! DONE SUCCESSFULLY. ')" ;
Character(39),  Parameter, Public :: Fmt_FL="('OOPS!!!  FAIL TO OPERATE PROPERLY.')" ;
Character(78),  Parameter, Public :: Fmt_ALLCT="('ERROR IN ALLOCATING Arrays. ERROR NUMBER IS :', I4, '   LOCATION: ??????.')" ;
Character(80),  Parameter, Public :: Fmt_DEALLCT="('ERROR IN DEALLOCATING Arrays. ERROR NUMBER IS :', I4, '   LOCATION: ??????.')" ;
Character(23),  Parameter, Public :: Fmt_RUNTIME="(A,F50.2,'   SECONDS')" ;
Character(70),  Parameter, Public :: Fmt_READ1="('ERROR IN READ STATEMENT. Unit IS : ',I5,' ERROR NUMBER IS : ', I5 )" ;
Character(76),  Parameter, Public :: Fmt_READ2="('End OF FILE IN READ STATEMENT. Unit IS : ',I5,' ERROR NUMBER IS : ', I5 )" ;
Character(78),  Parameter, Public :: Fmt_READ3="('End OF RECORD IN READ STATEMENT. Unit IS : ',I5,' ERROR NUMBER IS : ', I5 )" ;
Character(71),  Parameter, Public :: Fmt_Write1="('ERROR IN Write STATEMENT. Unit IS : ',I5,' ERROR NUMBER IS : ', I5 )" ;
Character(143), Parameter, Public :: Fmt_Element1="('Error in the element type. Either there is a mistake in the input file for element type or element type in not available in the code yet.')" ;
Character(144), Parameter, Public :: Fmt_Element2="('Error in the element type. This element number',I3,'is not available in the list of this code. Check the input file for element number',I19)" ;

! Unit NUMBERS OF EXTERNAL FILES ==================================================================
! Address file
Integer (Kind=Smll), Parameter, Public :: FileAdr=500 ! Address file that holds the model name & directories (.txt)

! Input files
Integer (Kind=Smll), Parameter, Public :: FileDataModel=501; ! Input file (.dataModel)
Integer (Kind=Smll), Parameter, Public :: UnInptXYZ=502; ! Input file for node coordinates (.XYZ)
Integer (Kind=Smll), Parameter, Public :: UnInptCnn=503; ! Input file for connectivities of elements (.Cnn)
Integer (Kind=Smll), Parameter, Public :: UnInptCnt=504; ! Input file for node constraints (.Cnt)

! Debugging files
Integer (Kind=Smll), Parameter, Public  :: Un_CHK=599; ! Scratch file for debugging (.Chk)

!Output files
Integer (Kind=Smll), Parameter, Public  :: FileInfo=600; ! Model information file (.Inf)


! Analysis case number ============================================================================
!Integer (Kind=Smll), Parameter, Public :: ACN_LI_ST=21; ! Linear Static Analysis


! Element Types Number ============================================================================
! Solid Element
!Integer (Kind=Tiny), Parameter, Public  :: El2d4NSldPS  =1; ! Element: 2D-4 noded-Solid

! PML Elements

! Spectral Element


! User defined types ==============================================================================
! Time signature of the model
type TimeDate_tp
  Integer (Kind=Smll)  :: Year, Month, Day ;               ! Date variables
  Integer (Kind=Smll)  :: Hour, Minute, Seconds, S100th ;  ! Time variables
end type TimeDate_tp

! Holds info. (name, Dir, output directory) of the model
type Input_Data_tp
  Character (Kind = 1, Len = 30 ) :: ModelName ;    ! Name of the model input file
  Character (Kind = 1, Len = 150) :: InputDir ;     ! Directory of the input file.
  Character (Kind = 1, Len = 150) :: AnalysisDir ;  ! Directory of Analysis input file.
  Character (Kind = 1, Len = 150) :: IntDir ;       ! Directory of internal files.
  Character (Kind = 1, Len = 150) :: AnalysisIntDir;! Directory of internal files for each analysis
  Character (Kind = 1, Len = 150) :: OutputDir;     ! Directory of output files (Results)
  Character (Kind = 1, Len = 150) :: AnalysisOutputDir;! Directory of output file for each analysis
  Character (Kind = 1, Len = 150), dimension(:), allocatable :: AnalysesNames;! Holds the name of the analysis input file

  Integer (Kind=Smll)  :: AnalysisType ;            ! Analysis Type: 1: 1D-2: 2D
  Integer (Kind=Smll)  :: OutputType ;              ! Output Type: 1: ordinary-2: HDF5
  Integer (Kind=Smll)  :: NumberOfAnalyses ;        ! Number of analysis
end type Input_Data_tp

Contains


End Module Parameters ;
