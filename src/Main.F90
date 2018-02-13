
!##################################################################################################
! Purpose: This code solves the 2D Shallow Water Equation
!
! Developed by: Babak Poursartip
! Supervised by: Clint Dawson
!
! The Institute for Computational Engineering and Sciences (ICES)
! The University of Texas at Austin
!
! ================================ V E R S I O N ==================================================
! V0.1: 02/13/2018  - Initiation.
!
! File version $Id
!
! Last update: 02/13/2018
!
! ================================ Global   V A R I A B L E S =====================================
!  . . . . . . . . . . . . . . . . Variables . . . . . . . . . . . . . . . . . . . . . . . . . . .
!
!##################################################################################################


program Shallow_Water_Equation;


! Libraries =======================================================================================

! Defined Modules =================================================================================
use Parameters;


! Global Variables ================================================================================
Implicit None ;
Include 'Global_Variables_Inc.F90'   ! All Global Variables are defined/described in this File




! Time and Date signature =========================================================================
Call CPU_TIME(Time_Start)
Call GETDAT(TimeDate%Year, TimeDate%Month, TimeDate%Day)
Call GETTIM(TimeDate%Hour, TimeDate%Minute, TimeDate%Seconds, TimeDate%S100th)

! Write some inFormation on screen
Write(*,*)" Numerical simulation of 2D Shallow water Equation"
Write(*,*)

! Directories, input, and output Files ============================================================
! Address File ------------------------------------------------------------------------------------
Write(*,fmt="(A)") " -Reading Address.txt file ..."

UnFile=File_Address
Open(Unit=UnFile, File='ADDRESS.txt', Err=1001, IOStat=IO_File, Access='SEQUENTIAL', &
     Action='READ', Asynchronous='NO', Blank='NULL', BLOCKSize=0, DisPOSE='Keep', &
     Form='FormATTED', Position='ASIS', Status='old')

! Read the input fine name and directories Form "ADDRESS_File.txt" in the current directory -------
Read(FileAdr,*)
Read(FileAdr,*) ModelInfo%ModelName
Read(FileAdr,*)
Read(FileAdr,*) ModelInfo%AnalysisType
Read(FileAdr,*)
Read(FileAdr,*) ModelInfo%OutputType
Read(FileAdr,*)
Read(FileAdr,*) ModelInfo%InputDir
Read(FileAdr,*)
Read(FileAdr,*) ModelInfo%IntDir
Read(FileAdr,*)
Read(FileAdr,*) ModelInfo%OutputDir
Read(FileAdr,*)
Read(FileAdr,*) ModelInfo%AnaName
Read(FileAdr,*) ModelInfo%NumberOfAnalyses

ModelInfo%AnalysisDir=Trim(AdjustL(ModelInfo%InputDir))//'/'//Trim(AdjustL(ModelName))//'/'//'Analysis'
ModelInfo%InputDir=Trim(AdjustL(ModelInfo%InputDir))//'/'//Trim(AdjustL(ModelName))//'/'//'Model'

Write (*,fmt="(2A)")" The model directory is: ", ModelInfo%InputDir
Write (*,fmt="(2A)")" The analysis name is: ", ModelInfo%AnalysisDir

! Create the results folder -----------------------------------------------------------------------
Write(*,fmt="(A)") " -Creating the output folders ..."

Directory=MakeDirQQ (Trim(AdjustL(ModelInfo%OutputDir))//'/'//Trim(AdjustL(ModelInfo%ModelName)))
  If (Directory) Then
    Write(*,fmt="(A)") "The result folder is created."
  Else
     Write (*,fmt="(A)") "The result folder already exists."
  End If

! Internal folder
Directory=MakeDirQQ (Trim(AdjustL (ModelInfo%IntDir))//'/'//Trim(AdjustL (ModelInfo%ModelName)))
  If (Directory) Then
     Write (*,fmt="(2A)") "The internal folder is created."
  Else ;
     Write (*,fmt="(2A)") "The internal folder already exists."
  End If ;

ModelInfo%OutputDir=Trim(AdjustL (ModelInfo%OutputDir))//'/'//Trim(AdjustL (ModelInfo%ModelName))
ModelInfo%IntDir=Trim(AdjustL (ModelInfo%IntDir))//'/'//Trim(AdjustL (ModelInfo%ModelName))

Write (*,fmt="(2A)")" The output directory is: ", ModelInfo%OutputDir

! Opening the information File --------------------------------------------------------------------
Write(*,fmt="(A)") " -Creating the info.txt file in the output folder ..."

UnFile=FileInfo
Open (Unit=UnFile, File=Trim(ModelInfo%ModelName)//.infM', &
      Err=1001, IOStat=IO_File, Access='SEQUENTIAL', Action='Write', Asynchronous='NO', &
      Blank='NULL', BLOCKSize=0, DEFAULTFile=Trim(OutDir), DisPOSE='Keep', Form='FormATTED', &
      Position='ASIS', Status='REPLACE' )

! Writing down the simulation time
Call Info(TimeDate, ModelInfo)

! Reading model data ==============================================================================
Write(*,        fmt="(A)") " -Opening the input file ..."
Write(FileInfo, fmt="(A)") " -Opening the input file ..."

UnFile=File_Input_Model ;
Open (Unit=UnFile, File=Trim(ModelInfo%ModelName)//'.dataModel', &
      Err=1001, IOStat=IO_File, Access='SEQUENTIAL', ACTION='READ', Asynchronous='NO', &
      Blank='NULL', BLOCKSize=0, DEFAULTFile=Trim(Model_InDir), DisPOSE='Keep', Form='Formatted', &
      Position='ASIS', Status='old' ) ;


Call CPU_TIME( TimeInputS ) ;

! Reading basic data: Ordinary OR HDF5 ------------------------------------------------------------
Write(*,        fmt="(A)") " -Reading the initial data file ..."
Write(FileInfo, fmt="(A)") " -Reading the initial data file ..."

  If (ModelInfo%OutputType==0_Smll) Then ! Ordinary
    Call Input(                                                          &
                                                                         & ! Integer (1) Variables
                                                                         & ! Integer (2) Variables
                                                                         & ! Integer (4) Variables
                                                                         & ! Integer (8) Variables
                                                                         & ! Real Variables
                                                                         & ! Integer Arrays
                                                                         & ! Real Arrays
                                                                         & ! Characters
                                                                         & ! Type
    )
  Else If (ModelInfo%OutputType==1_Smll) Then ! HDF5
    Call Input(                                                          &
                                                                         & ! Integer (1) Variables
                                                                         & ! Integer (2) Variables
                                                                         & ! Integer (4) Variables
                                                                         & ! Integer (8) Variables
                                                                         & ! Real Variables
                                                                         & ! Integer Arrays
                                                                         & ! Real Arrays
                                                                         & ! Characters
                                                                         & ! Type
    )
  End If

! Allocating required arrays
Write(*,        fmt="(A)") " -Allocating the required arrays ..."
Write(FileInfo, fmt="(A)") " -Allocating the required arrays ..."

Allocate ( ,
           STAT=Err_Alloc)
  If ( Err_Alloc /= 0 ) Then ;
    Write (*, Fmt_ALLCT) Err_Alloc ;  Write (FileInfo, Fmt_ALLCT) Err_Alloc ;
    Write(*, Fmt_FL) ;  Write(FileInfo, Fmt_FL) ; Write(*, Fmt_End) ; Read(*,*) ;  Stop ;
  End If

! Open required Files -----------------------------------------------------------------------------
Write(*,        fmt="(A)") " -Opening the input files ..."
Write(FileInfo, fmt="(A)") " -Opening the input files ..."
Include 'Open_File_Inc.F90'

! Reading input arrays ----------------------------------------------------------------------------
Write(*,        fmt="(A)") " -Reading arrays form data file ..."
Write(FileInfo, fmt="(A)") " -Reading arrays form data file ..."

  If (ModelInfo%OutputType==0_Smll) Then
    Call Input (                                                         &
                                                                         & ! Integer (1) Variables
                                                                         & ! Integer (2) Variables
                                                                         & ! Integer (4) Variables
                                                                         & ! Integer (8) Variables
                                                                         & ! Real Variables
                                                                         & ! Integer Arrays
                                                                         & ! Real Arrays
                                                                         & ! Characters
                                                                         & ! Type
    )
  Else If (ModelInfo%OutputType==1_Smll) Then
    Call Input_Binary (                                                  &
                                                                         & ! Integer (1) Variables
                                                                         & ! Integer (2) Variables
                                                                         & ! Integer (4) Variables
                                                                         & ! Integer (8) Variables
                                                                         & ! Real Variables
                                                                         & ! Integer Arrays
                                                                         & ! Real Arrays
                                                                         & ! Characters
                                                                         & ! Type
    )
  End If
Call CPU_TIME( TimeInputE )

! Close Input File --------------------------------------------------------------------------------
Write(*,        fmt="(A)") " -Closing input files ..."
Write(FileInfo, fmt="(A)") " -Closing input files ..."

! Close ADDRESS File
UnFile= FileAdr
Close ( Unit=UnFile, Status='Keep', Err=1002, IOStat=IO_File )

! Close data File
UnFile= UnInptAna
Close(Unit=UnFile, Status='Keep', Err=1002, IOStat=IO_File)

! close check File
!UnFile= Un_CHK ;
!Close (Unit=UnFile, Status='Keep', Err=1002, IOStat=IO_File) ;

! Simulations =====================================================================================
Write (*,        fmt="(A,I5)")"  Analysis Number: ", IAnalysis
Write (FileInfo, fmt="(A,I5)")"  Analysis Number: ", IAnalysis

Read(FileAdr,*) ModelInfo%AnalysisName ;
Write (*,        fmt="(2A)")"Analysis Name: ", ModelInfo%AnalysisName
Write (FileInfo, fmt="(2A)")"Analysis Name: ", ModelInfo%AnalysisName

! Opening the input file for this specific simulation
Write (*,        fmt="(A)") " -Opening the analysis file ..."
Write (FileInfo, fmt="(A)") " -Opening the analysis file ..."

UnFile=UnInptAna ;
Open (Unit=UnFile, File=Trim(ModelInfo%AnalysisName)//'.txt', Err= 1001, IOStat=IO_File, Access='SEQUENTIAL', &
      Action='READ', Asynchronous='NO', Blank='NULL', BLOCKSize=0, DEFAULTFile=Trim(Ana_InDir), &
      DisPOSE='Keep', FORM='FormATTED', Position='ASIS', Status='old') ;

! Creating the output file directory for this analysis --------------------------------------------
Write(*,        fmt="(A)") " -Creating the output folder for this analysis ..."
Write(FileInfo, fmt="(A)") " -Creating the output folder for this analysis ..."

Directory=MakeDirQQ (Trim(AdjustL (ModelInfo%OutputDir))//'/'//Trim(AdjustL (ModelInfo%AnalysisName))  ) ;
  If (Directory) Then ;
     Write (*,       fmt="(A)") "The output folder for this analysis created." ;
     Write (FileInfo,fmt="(A)") "The output folder for this analysis created." ;
  Else ;
     Write (*,        fmt="(A)") "The output folder for this analysis already exists." ;
     Write (FileInfo, fmt="(A)") "The output folder for this analysis already exists." ;
  End If ;

! Creating the internal File directory ------------------------------------------------------------
Directory=MakeDirQQ (Trim(AdjustL(ModelInfo%IntDir))//'/'//Trim(AdjustL(ModelInfo%AnalysisName))) ;
  If (Directory) Then
     Write (*,        fmt="(A)") "The internal folder for this analysis created."
     Write (FileInfo, fmt="(A)") "The internal folder for this analysis created."
  Else
     Write (*,        fmt="(A)") "The internal folder for this analysis already exists."
     Write (FileInfo, fmt="(A)") "The internal folder for this analysis already exists."
  End If

ModelInfo%AnalysisOutputDir=Trim(AdjustL(ModelInfo%OutputDir))//'/'//Trim(AdjustL(ModelInfo%AnalysisName))
ModelInfo%AnalysisIntDir=Trim(AdjustL(ModelInfo%IntDir))//'/'//Trim(AdjustL(ModelInfo%AnalysisName))

! Test File ---------------------------------------------------------------------------------------
!UnFile=UN_CHK ;
!Open ( Unit=UnFile, File=Trim(AnaName)//'_'//Trim(AdjustL(IndexSize))//'_'//Trim(AdjustL(IndexRank))//'.Chk', &
!     Err= 1001, IOStat=IO_File, Access='SEQUENTIAL', Action='Write', Asynchronous='NO', &
!     Blank='NULL', BLOCKSize=0, DEFAULTFile=Trim(InlDirAna), DisPOSE='Keep', Form='FormATTED', &
!     Position='ASIS', Status='REPLACE' ) ;


! Analysis ========================================================================================
  SELECT CASE (AnalysisType)

    CASE (N_1D_SWE)    ! # 1

      Include '1D_Shallow_Water.F90'

    ! Error in analysis numbering
    CASE DEFAULT
      Write(*,*)" The analysis type  is not available in this code. Modify the analysis type."
      Write(FileInfo,*)" The analysis type  is not available in this code. Modify the analysis type."
      Write(*,*)
      Write(FileInfo,*)
      Write(*,*)" Simulation terminated with error."
      Write(*, Fmt_End) ; Read(*,*) ;  Stop ;

  End SELECT

! Deallocating arrays
DEAllocate( ,      STAT = ERR_DeAlloc ) ;
  IF ( ERR_DeAlloc /= 0 ) Then ;
    Write (*, Fmt_DEALLCT) ERR_DeAlloc ;  Write (FileInfo, Fmt_DEALLCT) ERR_DeAlloc ;
    Write(*, Fmt_FL) ;  Write(FileInfo, Fmt_FL) ; Write(*, Fmt_End) ; Read(*,*) ;  STOP ;
  End If ;


! RUNNING TIME OF THE CODE ========================================================================
Call CPU_TIME ( TimeE ) ;
Call Info(NEl, TimeE, TimeS, TimeInputE, TimeInputS, TimeSolveE, TimeSolveS ) ;

Write (*,*)" Simulation was conducted successfully for:" ;
Write (*,"(' Model Name: ',A30,'Analysis Name: ', A30)")ModelName, AnaName ;
Write (*,*) ;

! Close Files -------------------------------------------------------------------------------------
! Close information File
UnFile= FileInfo ;
Close ( Unit=UnFile, Status='Keep', Err=1002, IOStat=IO_File ) ;


UnFile= UnInptAna ;
Close ( Unit=UnFile, Status='Keep', Err=1002, IOStat=IO_File ) ;

! End the code ====================================================================================
Write (*,"('Model Name: ',A30,'Analysis Name: ', A30)")ModelName, AnaName ;
Write (*, Fmt_SUC) ;  Write(FileInfo, Fmt_SUC) ;
Write (*, Fmt_End) ;

!#Read(*,*);
Stop ;


! =================================================================================================
! =================================================================================================
! Opening statement Errors
1001  If ( IO_File > 0 ) Then ;
        Write(*, Fmt_Err1_OPEN) UnFile, IO_File  ;  Write(FileInfo, Fmt_Err1_OPEN) UnFile, IO_File;
        Write(*, Fmt_FL) ; Write(FileInfo, Fmt_FL) ;
        Write(*, Fmt_End) ; Read(*,*) ;  Stop ;
      Else If ( IO_File < 0 ) Then ;
        Write(*, Fmt_Err1_OPEN) UnFile, IO_File  ;
        Write(FileInfo, Fmt_Err1_OPEN) UnFile, IO_File  ;  Write(*, Fmt_FL) ; Write(FileInfo, Fmt_FL);
        Write(*, Fmt_End) ; Read(*,*) ;  Stop ;
      End If ;


! Close statement Errors
1002  If ( IO_File > 0 ) Then ;
        Write(*, Fmt_Err1_Close) UnFile, IO_File  ;  Write(FileInfo, Fmt_Err1_Close) UnFile, IO_File ;
        Write(*, Fmt_FL) ; Write(FileInfo, Fmt_FL) ;
        Write(*, Fmt_End) ; Read(*,*) ; ; Stop ;
      End If ;

end program Shallow_Water_Equation;
