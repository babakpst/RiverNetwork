
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
! V0.1: 02/22/2018  - Initiation.
!
! File version $Id $
!
! Last update: 02/22/2018
!
! ================================ Global   V A R I A B L E S =====================================
!  . . . . . . . . . . . . . . . . Variables . . . . . . . . . . . . . . . . . . . . . . . . . . .
!
!##################################################################################################


program Shallow_Water_Equation


! Libraries =======================================================================================

! Defined Modules =================================================================================
use Parameters_mod
use Information_mod
use Input_mod
use ifport

! Global Variables ================================================================================
Implicit None

Include 'Global_Variables_Inc.f90'   ! All Global Variables are defined/described in this File

! Time and Date signature =========================================================================
Call cpu_time(SimulationTime%Time_Start)
Call GETDAT(TimeDate%Year, TimeDate%Month, TimeDate%Day)
Call GETTIM(TimeDate%Hour, TimeDate%Minute, TimeDate%Seconds, TimeDate%S100th)

call Header() ! Writes info on screen.

! Getting entered arguments =======================================================================
Arguments%ArgCount = command_argument_count()

! Allocating arg arrays
allocate(Arguments%Length(Arguments%ArgCount), Arguments%Arg(Arguments%ArgCount), Arguments%Argstatus(Arguments%ArgCount), STAT = ERR_Alloc)
  If (ERR_Alloc /= 0) Then
    write(*, Fmt_ALLCT) ERR_Alloc; write(FileInfo, Fmt_ALLCT) ERR_Alloc;
    write(*, Fmt_FL); write(FileInfo, Fmt_FL); read(*, Fmt_End); stop;
  End If


  do ii=1,Arguments%ArgCount
    call get_command_argument(ii, Arguments%Arg(ii), Arguments%Length(ii), Arguments%Argstatus(ii))
    !if (Arguments%Arg(1:2) /= "-") then
    !  write(*, fmt="(A)") " Wrong input argument! Using the default variables." ! <modify>
    !end if
  end do

! Directories, input, and output Files ============================================================
! Address File ------------------------------------------------------------------------------------
write(*,fmt="(A)") " -Reading Address.txt file ..."

call Input(ModelInfo)


! Opening the information File --------------------------------------------------------------------
write(*,fmt="(A)") " -Creating the info.txt file in the output folder ..."

UnFile=FileInfo
Open(Unit=UnFile, File=trim(ModelInfo%ModelName)//'.infM',     &
     Err=1001, IOStat=IO_File, Access='SEQUENTIAL', Action='write', Asynchronous='NO', &
     Blank='NULL', blocksize=0, defaultfile=trim(ModelInfo%OutputDir), DisPOSE='Keep', Form='formatted', &
     position='ASIS', status='REPLACE')

! Writing down the simulation time
Call Info(TimeDate, ModelInfo)

Call cpu_time(SimulationTime%Input_Starts)

! Reading model data ==============================================================================
write(*,        fmt="(A)") " -Reading the initial data file ..."
write(FileInfo, fmt="(A)") " -Reading the initial data file ..."

! Reading basic data: -----------------------------------------------------------------------------
call Input(ModelInfo, InitialInfo)

! Allocating required arrays
write(*,        fmt="(A)") " -Allocating the required arrays ..."
write(FileInfo, fmt="(A)") " -Allocating the required arrays ..."

!allocate( ,
!           STAT=Err_Alloc)
!  If (Err_Alloc /= 0) Then
!    write(*, Fmt_ALLCT) Err_Alloc; write(FileInfo, Fmt_ALLCT) Err_Alloc;
!    write(*, Fmt_FL); write(FileInfo, Fmt_FL); write(*, Fmt_End); read(*,*); stop;
!  End If

! Reading input arrays ----------------------------------------------------------------------------
write(*,        fmt="(A)") " -Reading arrays form data file ..."
write(FileInfo, fmt="(A)") " -Reading arrays form data file ..."

!call Input(                                                              &
!                                                                         & ! Integer (1) Variables
!                                                                         & ! Integer (2) Variables
!                                                                         & ! Integer (4) Variables
!                                                                         & ! Integer (8) Variables
!                                                                         & ! Real Variables
!                                                                         & ! Integer Arrays
!                                                                         & ! Real Arrays
!                                                                         & ! Characters
!                                                                         & ! Type
!    )

Call cpu_time(SimulationTime%Input_Ends)

! Close Input File --------------------------------------------------------------------------------
write(*,        fmt="(A)") " -Closing input files ..."
write(FileInfo, fmt="(A)") " -Closing input files ..."

! Close data File
UnFile= UnInptAna
Close(Unit=UnFile, status='Keep', Err=1002, IOStat=IO_File)

! close check File
!UnFile= Un_CHK
!Close(Unit=UnFile, status='Keep', Err=1002, IOStat=IO_File)

! Simulations =====================================================================================

  do i_analyses = 1, ModelInfo%NumberOfAnalyses


    ! Getting the required data for this specific analysis
    !call Input()

    ! Test File ---------------------------------------------------------------------------------------
    !UnFile=UN_CHK
    !Open( Unit=UnFile, File=trim(AnaName)//'_'//trim(AdjustL(IndexSize))//'_'//trim(AdjustL(IndexRank))//'.Chk', &
    !     Err= 1001, IOStat=IO_File, Access='SEQUENTIAL', Action='write', Asynchronous='NO', &
    !     Blank='NULL', blocksize=0, defaultfile=trim(InlDirAna), DisPOSE='Keep', Form='formatted', &
    !     position='ASIS', status='REPLACE')


    ! Analysis ========================================================================================
      SELECT CASE(ModelInfo%AnalysisType)

        CASE(AnalysisType_1D)    ! # 1

          ! <modify>

        ! Error in analysis numbering
        CASE DEFAULT
          write(*,*)" The analysis type  is not available in this code. Modify the analysis type."
          write(FileInfo,*)" The analysis type  is not available in this code. Modify the analysis type."
          write(*,*)
          write(FileInfo,*)
          write(*,*)" Simulation terminated with error."
          write(*, Fmt_End);  read(*,*);   stop;

      End SELECT

    write(*,*)" Simulation was conducted successfully for:"
    write(*,"(' Model Name: ',A30,'Analysis Name: ', A30)")ModelInfo%ModelName, ModelInfo%AnalysesNames(i_analyses)
    write(*,*)

  end do


! Deallocating arrays
DEallocate(Arguments%Length, Arguments%Arg, Arguments%Argstatus,      STAT = ERR_DeAlloc )
  IF (ERR_DeAlloc /= 0) Then
    write(*, Fmt_DEALLCT) ERR_DeAlloc; write(FileInfo, Fmt_DEALLCT) ERR_DeAlloc;
    write(*, Fmt_FL); write(FileInfo, Fmt_FL);  write(*, Fmt_End);  read(*,*);   stop;
  End If


! RUNNING TIME OF THE CODE ========================================================================
Call cpu_time(SimulationTime%Time_End)
!Call Info()



! Close Files -------------------------------------------------------------------------------------
! Close information File
UnFile= FileInfo
Close(Unit=UnFile, status='Keep', Err=1002, IOStat=IO_File)


UnFile= UnInptAna
Close(Unit=UnFile, status='Keep', Err=1002, IOStat=IO_File)

! End the code ====================================================================================
write(*, Fmt_SUC); write(FileInfo, Fmt_SUC);
write(*, Fmt_End)

!#read(*,*)
stop


! Errors ==========================================================================================
! Opening statement Errors
1001  If (IO_File > 0) Then
        write(*, Fmt_Err1_OPEN) UnFile, IO_File; write(FileInfo, Fmt_Err1_OPEN) UnFile, IO_File;
        write(*, Fmt_FL); write(FileInfo, Fmt_FL);
        write(*, Fmt_End); read(*,*); stop;
      Else If (IO_File < 0) Then
        write(*, Fmt_Err1_OPEN) UnFile, IO_File;
        write(FileInfo, Fmt_Err1_OPEN) UnFile, IO_File; write(*, Fmt_FL); write(FileInfo, Fmt_FL);
        write(*, Fmt_End);  read(*,*);   stop;
      End If


! Close statement Errors
1002  If (IO_File > 0) Then
        write(*, Fmt_Err1_Close) UnFile, IO_File; write(FileInfo, Fmt_Err1_Close) UnFile, IO_File;
        write(*, Fmt_FL); write(FileInfo, Fmt_FL);
        write(*, Fmt_End); read(*,*);   stop;
      End If

end program Shallow_Water_Equation
