
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
! File version $Id $
!
! Last update: 02/13/2018
!
! ================================ Global   V A R I A B L E S =====================================
!  . . . . . . . . . . . . . . . . Variables . . . . . . . . . . . . . . . . . . . . . . . . . . .
!
!##################################################################################################


program Shallow_Water_Equation


! Libraries =======================================================================================

! Defined Modules =================================================================================
use Parameters


! Global Variables ================================================================================
Implicit None

Include 'Global_Variables_Inc.F90'   ! All Global Variables are defined/described in this File

! Time and Date signature =========================================================================
Call CPU_TIME(Time_Start)
Call GETDAT(TimeDate%Year, TimeDate%Month, TimeDate%Day)
Call GETTIM(TimeDate%Hour, TimeDate%Minute, TimeDate%Seconds, TimeDate%S100th)

! Write some inFormation on screen
Write(*,*)" Numerical simulation of 2D Shallow water Equation"
Write(*,*)

! Getting entered arguments =======================================================================
Arguments%ArgCount = command_argument_count()

! Allocating arg arrays
Allocate (Arguments%Length(Arguments%ArgCount), Arguments%Arg(Arguments%ArgCount), Arguments%ArgStatus(Arguments%ArgCount), STAT = ERR_Alloc)
  If ( ERR_Alloc /= 0 ) Then
    Write (*, Fmt_ALLCT) ERR_Alloc   Write (UnInf, Fmt_ALLCT) ERR_Alloc
    Write(*, Fmt_FL)   Write(UnInf, Fmt_FL)  Read(*, Fmt_End)   Stop
  End If


  do ii=1,Arguments%ArgCount
    call get_command_argument(ii, Arguments%Arg(ii), Arguments%Length(ii), Arguments%ArgStatus(ii))
    if ( arg(1:1) /= "-" ) then
      write(*, fmt="(A)") " Wrong input argument! Using the default variables." ! <modify>
    end if
  end do

! Directories, input, and output Files ============================================================
! Address File ------------------------------------------------------------------------------------
Write(*,fmt="(A)") " -Reading Address.txt file ..."

call Input(ModelInfo)





! Opening the information File --------------------------------------------------------------------
Write(*,fmt="(A)") " -Creating the info.txt file in the output folder ..."

UnFile=FileInfo
Open (Unit=UnFile, File=Trim(ModelInfo%ModelName)//.infM', &
      Err=1001, IOStat=IO_File, Access='SEQUENTIAL', Action='Write', Asynchronous='NO', &
      Blank='NULL', BLOCKSize=0, DEFAULTFile=Trim(OutDir), DisPOSE='Keep', Form='FormATTED', &
      Position='ASIS', Status='REPLACE' )

! Writing down the simulation time
Call Info(TimeDate, ModelInfo)

Call CPU_TIME( TimeInputS )

! Reading model data ==============================================================================
Write(*,        fmt="(A)") " -Reading the initial data file ..."
Write(FileInfo, fmt="(A)") " -Reading the initial data file ..."

! Reading basic data: Ordinary OR HDF5 ------------------------------------------------------------
call Input(                                                          &
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


! Allocating required arrays
Write(*,        fmt="(A)") " -Allocating the required arrays ..."
Write(FileInfo, fmt="(A)") " -Allocating the required arrays ..."

Allocate ( ,
           STAT=Err_Alloc)
  If ( Err_Alloc /= 0 ) Then
    Write (*, Fmt_ALLCT) Err_Alloc   Write (FileInfo, Fmt_ALLCT) Err_Alloc
    Write(*, Fmt_FL)   Write(FileInfo, Fmt_FL)  Write(*, Fmt_End)  Read(*,*)   Stop
  End If

! Open required Files -----------------------------------------------------------------------------
Write(*,        fmt="(A)") " -Opening the input files ..."
Write(FileInfo, fmt="(A)") " -Opening the input files ..."
Include 'Open_File_Inc.F90'

! Reading input arrays ----------------------------------------------------------------------------
Write(*,        fmt="(A)") " -Reading arrays form data file ..."
Write(FileInfo, fmt="(A)") " -Reading arrays form data file ..."

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

Call CPU_TIME( TimeInputE )

! Close Input File --------------------------------------------------------------------------------
Write(*,        fmt="(A)") " -Closing input files ..."
Write(FileInfo, fmt="(A)") " -Closing input files ..."

! Close data File
UnFile= UnInptAna
Close(Unit=UnFile, Status='Keep', Err=1002, IOStat=IO_File)

! close check File
!UnFile= Un_CHK
!Close (Unit=UnFile, Status='Keep', Err=1002, IOStat=IO_File)

! Simulations =====================================================================================

  do i_analysis = 1, ModelInfo%NumberOfAnalyses


    ! Getting the required data for this specific analysis
    call Input()

    ! Test File ---------------------------------------------------------------------------------------
    !UnFile=UN_CHK
    !Open ( Unit=UnFile, File=Trim(AnaName)//'_'//Trim(AdjustL(IndexSize))//'_'//Trim(AdjustL(IndexRank))//'.Chk', &
    !     Err= 1001, IOStat=IO_File, Access='SEQUENTIAL', Action='Write', Asynchronous='NO', &
    !     Blank='NULL', BLOCKSize=0, DEFAULTFile=Trim(InlDirAna), DisPOSE='Keep', Form='FormATTED', &
    !     Position='ASIS', Status='REPLACE' )


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
          Write(*, Fmt_End)  Read(*,*)   Stop

      End SELECT

  end do


! Deallocating arrays
DEAllocate(Arguments%Length, Arguments%Arg, Arguments%ArgStatus,      STAT = ERR_DeAlloc )
  IF ( ERR_DeAlloc /= 0 ) Then
    Write (*, Fmt_DEALLCT) ERR_DeAlloc   Write (FileInfo, Fmt_DEALLCT) ERR_DeAlloc
    Write(*, Fmt_FL)   Write(FileInfo, Fmt_FL)  Write(*, Fmt_End)  Read(*,*)   STOP
  End If


! RUNNING TIME OF THE CODE ========================================================================
Call CPU_TIME ( TimeE )
Call Info(NEl, TimeE, TimeS, TimeInputE, TimeInputS, TimeSolveE, TimeSolveS )

Write (*,*)" Simulation was conducted successfully for:"
Write (*,"(' Model Name: ',A30,'Analysis Name: ', A30)")ModelName, AnaName
Write (*,*)

! Close Files -------------------------------------------------------------------------------------
! Close information File
UnFile= FileInfo
Close ( Unit=UnFile, Status='Keep', Err=1002, IOStat=IO_File )


UnFile= UnInptAna
Close ( Unit=UnFile, Status='Keep', Err=1002, IOStat=IO_File )

! End the code ====================================================================================
Write (*,"('Model Name: ',A30,'Analysis Name: ', A30)")ModelName, AnaName
Write (*, Fmt_SUC)   Write(FileInfo, Fmt_SUC)
Write (*, Fmt_End)

!#Read(*,*)
Stop


! Errors ==========================================================================================
! Opening statement Errors
1001  If ( IO_File > 0 ) Then
        Write(*, Fmt_Err1_OPEN) UnFile, IO_File    Write(FileInfo, Fmt_Err1_OPEN) UnFile, IO_File
        Write(*, Fmt_FL)  Write(FileInfo, Fmt_FL)
        Write(*, Fmt_End)  Read(*,*)   Stop
      Else If ( IO_File < 0 ) Then
        Write(*, Fmt_Err1_OPEN) UnFile, IO_File
        Write(FileInfo, Fmt_Err1_OPEN) UnFile, IO_File    Write(*, Fmt_FL)  Write(FileInfo, Fmt_FL)
        Write(*, Fmt_End)  Read(*,*)   Stop
      End If


! Close statement Errors
1002  If ( IO_File > 0 ) Then
        Write(*, Fmt_Err1_Close) UnFile, IO_File    Write(FileInfo, Fmt_Err1_Close) UnFile, IO_File
        Write(*, Fmt_FL)  Write(FileInfo, Fmt_FL)
        Write(*, Fmt_End)  Read(*,*)   Stop
      End If

end program Shallow_Water_Equation
