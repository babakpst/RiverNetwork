
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
! V0.00: 02/16/2018  - Initiation.
! V0.01: 02/18/2018  - Compiled for the first time.
! V0.02: 02/23/2018  - Adding input modules
! V0.02: 02/24/2018  - Adding input modules
! V0.02: 02/26/2018  - Adding discretize module
! V0.03: 03/02/2018  - Adding the result module
! V0.04: 03/02/2018  - Adding the solver module
! V0.10: 03/08/2018  - Initiated: Compiled without error.
! V0.11: 03/09/2018  - Adding limiter to the code
!
! File version $Id $
!
! Last update: 03/20/2018
!
! ================================ Global   V A R I A B L E S =====================================
!  . . . . . . . . . . . . . . . . Variables . . . . . . . . . . . . . . . . . . . . . . . . . . .
!
!##################################################################################################


program Shallow_Water_Equation


! Libraries =======================================================================================
use ifport

! Defined Modules =================================================================================
use Parameters_mod
use Information_mod
use Input_mod
use Discretization_mod
use LaxWendroff_mod

! Global Variables ================================================================================
Implicit None

Include 'Global_Variables_Inc.f90'   ! All Global Variables are defined/described in this File
ModelInfo%Version = 0.1_SGL          ! Reports the version of the code

! Time and Date signature =========================================================================
Call cpu_time(SimulationTime%Time_Start)
Call GETDAT(TimeDate%Year, TimeDate%Month, TimeDate%Day)
Call GETTIM(TimeDate%Hour, TimeDate%Minute, TimeDate%Seconds, TimeDate%S100th)

call Header(ModelInfo%Version) ! Writes info on screen.

! Getting entered arguments =======================================================================
Arguments%ArgCount = command_argument_count()

! Allocating arg arrays
allocate(Arguments%Length(Arguments%ArgCount), Arguments%Arg(Arguments%ArgCount), Arguments%Argstatus(Arguments%ArgCount), stat = ERR_Alloc)
  if (ERR_Alloc /= 0) then
    write(*, Fmt_ALLCT) ERR_Alloc; write(FileInfo, Fmt_ALLCT) ERR_Alloc;
    write(*, Fmt_FL); write(FileInfo, Fmt_FL); read(*, Fmt_End); stop;
  end if


  do ii=1,Arguments%ArgCount
    call get_command_argument(ii, Arguments%Arg(ii), Arguments%Length(ii), Arguments%Argstatus(ii))
    !if (Arguments%Arg(1:2) /= "-") then
    !  write(*, fmt="(A)") " Wrong input argument! Using the default variables." ! <modify>
    !end if
  end do

! Directories, input, and output Files ============================================================
! Address File ------------------------------------------------------------------------------------
write(*,        fmt="(A)") " -Reading Address.txt file ..."
!write(FileInfo, fmt="(A)") " -Reading Address.txt file ..."

call Input(ModelInfo)


! Opening the information File --------------------------------------------------------------------
write(*,        fmt="(A)") " -Creating the info.txt file in the output folder ..."
!write(FileInfo, fmt="(A)") " -Creating the info.txt file in the output folder ..."

UnFile=FileInfo
Open(Unit=UnFile, File=trim(ModelInfo%ModelName)//'.infM',     &
     Err=1001, IOstat=IO_File, Access='SEQUENTIAL', Action='write', Asynchronous='NO', &
     Blank='NULL', blocksize=0, defaultfile=trim(ModelInfo%OutputDir), DisPOSE='Keep', Form='formatted', &
     position='ASIS', status='REPLACE')

! Writing down the simulation time
Call Info(TimeDate, ModelInfo)

Call cpu_time(SimulationTime%Input_Starts)

! Reading model data ==============================================================================
write(*,        fmt="(A)") " -Reading the initial data file ..."
write(FileInfo, fmt="(A)") " -Reading the initial data file ..."

! Reading basic data: -----------------------------------------------------------------------------
call Input_Basic_sub(ModelInfo, Geometry)

! Allocating required arrays
write(*,        fmt="(A)") " -Allocating the required arrays ..."
write(FileInfo, fmt="(A)") " -Allocating the required arrays ..."

allocate(Geometry%ReachLength(Geometry%NoReaches), Geometry%ReachDisc(Geometry%NoReaches), &
         Geometry%ReachType(Geometry%NoReaches), Geometry%ReachSlope(Geometry%NoReaches),  &
         Geometry%ReachManning(Geometry%NoReaches), Geometry%ReachWidth(Geometry%NoReaches),&
         stat=Err_Alloc)
  if (Err_Alloc /= 0) then
    write(*, Fmt_ALLCT) Err_Alloc; write(FileInfo, Fmt_ALLCT) Err_Alloc;
    write(*, Fmt_FL); write(FileInfo, Fmt_FL); write(*, Fmt_End); read(*,*); stop;
  end if

! Reading input arrays ----------------------------------------------------------------------------
write(*,        fmt="(A)") " -Reading arrays form data file ..."
write(FileInfo, fmt="(A)") " -Reading arrays form data file ..."

! Geometry
call Input_Array_sub(ModelInfo, Geometry)

Call cpu_time(SimulationTime%Input_Ends)

! close check File
!UnFile= Un_CHK
!Close(Unit=UnFile, status='Keep', Err=1002, IOstat=IO_File)

! Discretization ----------------------------------------------------------------------------------
write(*,        fmt="(A)") " -Discretization ..."
write(FileInfo, fmt="(A)") " -Discretization ..."
print*," check 001"
call Discretize(Geometry, Discretization, ModelInfo)
print*," check 000"
! Simulations =====================================================================================

  do i_analyses = 1, ModelInfo%NumberOfAnalyses

    write(*,        fmt="(A,I10)") " -Analyse no.", i_analyses
    write(FileInfo, fmt="(A,I10)") " -Analyse no.", i_analyses

    ! Getting the required data for this specific analysis
    call Input(i_analyses, ModelInfo, AnalysisInfo)

    ! Test File -----------------------------------------------------------------------------------
    !UnFile=UN_CHK
    !Open( Unit=UnFile, File=trim(AnaName)//'_'//trim(AdjustL(IndexSize))//'_'//trim(AdjustL(IndexRank))//'.Chk', &
    !     Err= 1001, IOstat=IO_File, Access='SEQUENTIAL', Action='write', Asynchronous='NO', &
    !     Blank='NULL', blocksize=0, defaultfile=trim(InlDirAna), DisPOSE='Keep', Form='formatted', &
    !     position='ASIS', status='REPLACE')

    ! Analysis ====================================================================================
      select case(AnalysisInfo%AnalysisType)

        case(AnalysisType_1D)    ! # 1: Richtmyer

          allocate(Richtmyer(NCells=Discretization%NCells) :: Experiment_TypeI,     stat=ERR_Alloc)
            if (ERR_Alloc /= 0) then
              write (*, Fmt_ALLCT) ERR_Alloc;  write (FileInfo, Fmt_ALLCT) ERR_Alloc;
              write(*, Fmt_FL);  write(FileInfo, Fmt_FL); read(*, Fmt_End);  stop;
            end if


          Experiment_TypeI%ModelInfo = ModelInfo
          Experiment_TypeI%AnalysisInfo = AnalysisInfo
          Experiment_TypeI%Discretization = Discretization
          call Experiment_TypeI%Solve()

        case(AnalysisType_1D)    ! # 2: Lax-Wendroff with limiter in combination with upwind method

          allocate(SolverWithLimiter(NCells=Discretization%NCells) :: Experiment_TypeII,     stat=ERR_Alloc)
            if (ERR_Alloc /= 0) then
              write (*, Fmt_ALLCT) ERR_Alloc;  write (FileInfo, Fmt_ALLCT) ERR_Alloc;
              write(*, Fmt_FL);  write(FileInfo, Fmt_FL); read(*, Fmt_End);  stop;
            end if

          Experiment_TypeII%ModelInfo = ModelInfo
          Experiment_TypeII%AnalysisInfo = AnalysisInfo
          Experiment_TypeII%Discretization = Discretization
          call Experiment_TypeII%Solve()

        ! Error in analysis numbering
        case default
          write(*,*)" The analysis type  is not available in this code. Modify the analysis type."
          write(FileInfo,*)" The analysis type  is not available in this code. Modify the analysis type."
          write(*,*)
          write(FileInfo,*)
          write(*,*)" Simulation terminated with error."
          write(*, Fmt_End);  read(*,*);   stop;

      end select

    write(*,*)" Simulation was conducted successfully for:"
    write(*,"(' Model Name: ',A30,'Analysis Name: ', A30)")ModelInfo%ModelName, ModelInfo%AnalysesNames(i_analyses)
    write(*,*)

  end do


! Deallocating arrays
DEallocate(Arguments%Length, Arguments%Arg, Arguments%Argstatus,      stat = ERR_DeAlloc )
  if (ERR_DeAlloc /= 0) then
    write(*, Fmt_DEALLCT) ERR_DeAlloc; write(FileInfo, Fmt_DEALLCT) ERR_DeAlloc;
    write(*, Fmt_FL); write(FileInfo, Fmt_FL);  write(*, Fmt_End);  read(*,*);   stop;
  end if


! RUNNING TIME OF THE CODE ========================================================================
Call cpu_time(SimulationTime%Time_End)
!Call Info()

! End the code ====================================================================================
write(*, Fmt_SUC); write(FileInfo, Fmt_SUC);
write(*, Fmt_End)

! Close Files -------------------------------------------------------------------------------------
! Close information File
UnFile= FileInfo
Close(Unit=UnFile, status='Keep', Err=1002, IOstat=IO_File)

UnFile= UnInptAna
Close(Unit=UnFile, status='Keep', Err=1002, IOstat=IO_File)




!#read(*,*)
stop


! Errors ==========================================================================================
! Opening statement Errors
1001  if (IO_File > 0) then
        write(*, Fmt_Err1_OPEN) UnFile, IO_File; write(FileInfo, Fmt_Err1_OPEN) UnFile, IO_File;
        write(*, Fmt_FL); write(FileInfo, Fmt_FL);
        write(*, Fmt_End); read(*,*); stop;
      Else if (IO_File < 0) then
        write(*, Fmt_Err1_OPEN) UnFile, IO_File;
        write(FileInfo, Fmt_Err1_OPEN) UnFile, IO_File; write(*, Fmt_FL); write(FileInfo, Fmt_FL);
        write(*, Fmt_End);  read(*,*);   stop;
      end if


! Close statement Errors
1002  if (IO_File > 0) then
        write(*, Fmt_Err1_Close) UnFile, IO_File; write(FileInfo, Fmt_Err1_Close) UnFile, IO_File;
        write(*, Fmt_FL); write(FileInfo, Fmt_FL);
        write(*, Fmt_End); read(*,*);   stop;
      end if

end program Shallow_Water_Equation
