
!##################################################################################################
! Purpose: This code solves the 1D and 2D Shallow Water Equations, in parallel, using the hybrid
!          programming (OMP and MPI)
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
! V0.12: 03/20/2018  - Debugging the code with limiter
! V1.00: 04/10/2018  - Cleaning the code after having the right results
! V2.00: 04/18/2018  - Parallelization using OMP
! V2.10: 04/23/2018  - Time module
! V3.00: 05/16/2018  - MPI parallelization
!
! File version $Id $
!
! Last update: 05/16/2018
!
! ================================ Global   V A R I A B L E S =====================================
!  . . . . . . . . . . . . . . . . Variables . . . . . . . . . . . . . . . . . . . . . . . . . . .
!
!##################################################################################################

program Shallow_Water_Equations


! Libraries =======================================================================================
use MPI

! Defined Modules =================================================================================
use Parameters_mod
use Timer_mod
use Information_mod
use Input_mod
use Model_mod
use Solver_mod

! Global Variables ================================================================================
Implicit None

Include 'Global_Variables_Inc.f90'   ! All Global Variables are defined/described in this File
ModelInfo%Version = 2.0_SGL          ! Reports the version of the code

! Initializing the MPI ============================================================================
write(*,*) " Initializing MPI ..."   ! <MPI>

call MPI_Init(MPI_err) ! <MPI>
call MPI_Comm_Size(MPI_COMM_WORLD, ModelInfo%size, MPI_err) ! <MPI>
call MPI_Comm_Rank(MPI_COMM_WORLD, ModelInfo%rank, MPI_err) ! <MPI>

!write(*,fmt="Hello from rank: ", I6, "- We are a total of:", I6) rank,size
if ( ModelInfo%rank == 0) write(*,fmt="(' ****** Total number of ranks: ', I6)") ModelInfo%size

! Time and Date signature =========================================================================
call system_clock(TotalTime%startSys, TotalTime%clock_rate)
call TotalTime%start()
call GETDAT(TimeDate%Year, TimeDate%Month, TimeDate%Day)
call GETTIM(TimeDate%Hour, TimeDate%Minute, TimeDate%Seconds, TimeDate%S100th)

if ( ModelInfo%rank == 1) call Header(ModelInfo%Version) ! Writes info on screen.

! Getting entered arguments =======================================================================
Arguments%ArgCount = command_argument_count()

! Allocating arg arrays
allocate(Arguments%Length(Arguments%ArgCount), Arguments%Arg(Arguments%ArgCount), &
         Arguments%Argstatus(Arguments%ArgCount), stat = ERR_Alloc)
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
write(*,         fmt="(A)") " -Reading Address.txt file ..."
!write(FileInfo, fmt="(A)") " -Reading Address.txt file ..."

call ModelInfo%Input()

! Opening the information File --------------------------------------------------------------------
write(*,        fmt="(A)") " -Creating the info.txt file in the output folder ..."
!write(FileInfo, fmt="(A)") " -Creating the info.txt file in the output folder ..."

UnFile=FileInfo
Open(Unit=UnFile, File=trim(ModelInfo%ModelName)//'.infM', &
     Err=1001, IOstat=IO_File, Access='SEQUENTIAL', Action='write', Asynchronous='NO', &
     Blank='NULL', blocksize=0, defaultfile=trim(ModelInfo%OutputDir), DisPOSE='keep',  &
     Form='formatted', position='ASIS', status='replace')

! Writing down the simulation time
Call Info(TimeDate, ModelInfo)

call InputTime%start()

! Reading model data from the partitioned file ====================================================
write(*,        fmt="(A)") " -Reading the input file ..."
write(FileInfo, fmt="(A)") " -Reading the input file ..."

call Discretization%Input(ModelInfo)

call InputTime%stop()

! Simulations =====================================================================================
  do i_analyses = 1, ModelInfo%NumberOfAnalyses

    write(*,        fmt="(A,I10)") " -Analyse no.", i_analyses
    write(FileInfo, fmt="(A,I10)") " -Analyse no.", i_analyses

    ! Getting the required data for this specific analysis
    call AnalysisInfo%Analysis(i_analyses, ModelInfo)

    ! Analysis ====================================================================================
      select case(AnalysisInfo%AnalysisType)

        case(AnalysisType_1D_Limiter) !#2: Lax-Wendroff with limiter in combination with upwind

          Experiment_TypeII%ModelInfo = ModelInfo
          Experiment_TypeII%AnalysisInfo = AnalysisInfo
          Experiment_TypeII%Discretization = Discretization

          call SimulationTime%start()
          call Experiment_TypeII%Solve(TotalTime)
          call SimulationTime%stop()

        ! Error in analysis numbering
        case default
          write(*,*)       " The analysis type  is not available in this code. Modify the &
                            analysis type."
          write(FileInfo,*)" The analysis type  is not available in this code. Modify the &
                            analysis type."
          write(*,*)
          write(FileInfo,*)
          write(*,*)" Simulation terminated with error."
          write(*, Fmt_End);  read(*,*);   stop;

      end select

    write(*,*)" Simulation was conducted successfully for:"
    write(*,"(' Model Name: ',A30,'Analysis Name: ', A30)") &
              ModelInfo%ModelName, ModelInfo%AnalysesNames(i_analyses)
    write(*,*)

  end do

! Deallocating arrays
DEallocate(Arguments%Length, Arguments%Arg, Arguments%Argstatus,      stat = ERR_DeAlloc )
  if (ERR_DeAlloc /= 0) then
    write(*, Fmt_DEALLCT) ERR_DeAlloc; write(FileInfo, Fmt_DEALLCT) ERR_DeAlloc;
    write(*, Fmt_FL); write(FileInfo, Fmt_FL);  write(*, Fmt_End);  read(*,*);   stop;
  end if

! RUNNING TIME OF THE CODE ========================================================================
call TotalTime%stop()
call system_clock(TotalTime%endSys, TotalTime%clock_rate)

write(*,        fmt="(' Input time: ',F23.10 , ' seconds.' )")  InputTime%elapsedTime()
write(FileInfo, fmt="(' Input time: ',F23.10 , ' seconds.' )")  InputTime%elapsedTime()

write(*,        fmt="(' Simulation time: ',F23.10 , ' seconds.' )") &
                                                                      SimulationTime%elapsedTime()
write(FileInfo, fmt="(' Simulation time: ',F23.10 , ' seconds.' )") &
                                                                      SimulationTime%elapsedTime()

write(*,        fmt="(' Total simulation time: ',F23.10 , ' seconds.' )")  TotalTime%elapsedTime()
write(FileInfo, fmt="(' Total simulation time: ',F23.10 , ' seconds.' )")  TotalTime%elapsedTime()


write(*,        fmt="(' FINAL SIMULATION TIME: ',F23.10 , ' seconds.' )") &
                               real(TotalTime%endSys-TotalTime%startSys)/real(TotalTime%clock_rate)

! End the code ====================================================================================
write(*, Fmt_SUC); write(FileInfo, Fmt_SUC);
write(*, Fmt_End)

! Close Files -------------------------------------------------------------------------------------
! Close information File
UnFile= FileInfo
Close(Unit=UnFile, status='keep', Err=1002, IOstat=IO_File)

UnFile= UnInptAna
Close(Unit=UnFile, status='keep', Err=1002, IOstat=IO_File)

! Concluding the MPI ==============================================================================
call MPI_Finalize(MPI_err) ! <MPI>

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

end program Shallow_Water_Equations
