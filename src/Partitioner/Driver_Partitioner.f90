
!##################################################################################################
! Purpose: This code partitions the domain for main simulator.
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
! V2.00: 04/17/2018  - Developing the partitioner code.
!
! File version $Id $
!
! Last update: 04/17/2018
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
use Model_mod
use Discretization_mod
use Partitioner_mod

! Global Variables ================================================================================
Implicit None

Include 'Global_Variables_Inc.f90'   ! All Global Variables are defined/described in this File
ModelInfo%Version = 2.0_SGL          ! Reports the version of the code

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
write(*,         fmt="(A)") " -Reading Address.txt file ..."
!write(FileInfo, fmt="(A)") " -Reading Address.txt file ..."

call ModelInfo%Input()

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
call Geometry%Basic(ModelInfo)

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
call Geometry%Array(ModelInfo)

Call cpu_time(SimulationTime%Input_Ends)

! close check File
!UnFile= Un_CHK
!Close(Unit=UnFile, status='Keep', Err=1002, IOstat=IO_File)

! Discretization ----------------------------------------------------------------------------------
write(*,        fmt="(A)") " -Discretization ..."
write(FileInfo, fmt="(A)") " -Discretization ..."

call Discretization%Discretize(Geometry, ModelInfo)

! Partitioning and writing results
call Partitioner_1D_Sub(Geometry, Discretization, ModelInfo)

! Deallocating arrays
DEallocate(Arguments%Length, Arguments%Arg, Arguments%Argstatus,      stat = ERR_DeAlloc )
  if (ERR_DeAlloc /= 0) then
    write(*, Fmt_DEALLCT) ERR_DeAlloc; write(FileInfo, Fmt_DEALLCT) ERR_DeAlloc;
    write(*, Fmt_FL); write(FileInfo, Fmt_FL);  write(*, Fmt_End);  read(*,*);   stop;
  end if

! Running time of the code ========================================================================
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
