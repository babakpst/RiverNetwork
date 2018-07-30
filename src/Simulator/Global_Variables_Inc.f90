
! Integer Variables -------------------------------------------------------------------------------

integer(kind=Smll) :: UnFile=0_smll      ! Holds Unit of a file for error message
integer(kind=Smll) :: IO_File=0_smll     ! For IOSTAT: Input Output status in OPEN command
integer(kind=Smll) :: ERR_Alloc=0_smll   ! Allocating and DeAllocating errors
integer(kind=Smll) :: ERR_DeAlloc=0_smll ! Allocating and DeAllocating errors
integer(kind=Smll) :: i_analyses=0_smll  ! loop index to read the analyses files
integer(kind=Smll) :: ii, jj             ! Loop index

! MPI parameters
integer :: MPI_err

! - Type ------------------------------------------------------------------------------------------
type(TimeDate_tp) :: TimeDate       ! Indicates the time and date of simulation
type(Timer_tp)    :: InputTime, SimulationTime, TotalTime ! Holds the running time

type(ArgCommands)     :: Arguments  ! Holds the entered arguments from the command line
type(Input_Data_tp)   :: ModelInfo  ! Holds info. (name, dir, output dir) of the model
type(AnalysisData_tp) :: AnalysisInfo ! Holds initial data required for array allocation

! Holds the entire information required of the Model
type(DiscretizedNetwork_tp) :: Model
type(SolverWithLimiter_tp)  :: Experiment_TypeII !info to solve shallow water equation with limiter

