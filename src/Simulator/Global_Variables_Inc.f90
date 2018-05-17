
! Integer Variables -------------------------------------------------------------------------------

integer(kind=Smll) :: UnFile        ! Holds Unit of a file for error message
integer(kind=Smll) :: IO_File       ! For IOSTAT: Input Output status in OPEN command
integer(kind=Smll) :: ERR_Alloc, ERR_DeAlloc ! Allocating and DeAllocating errors
integer(kind=Smll) :: i_analyses    ! loop index to read the analyses files
integer(kind=Smll) :: ii, jj        ! Loop index

! MPI parameters
integer :: MPI_err

! - Type ------------------------------------------------------------------------------------------
type(TimeDate_tp) :: TimeDate       ! Indicates the time and date of simulation
type(Timer_tp)    :: InputTime, SimulationTime, TotalTime ! Holds the running time

type(ArgCommands)      :: Arguments  ! Holds the entered arguments from the command line
type(Input_Data_tp)    :: ModelInfo  ! Holds info. (name, dir, output dir) of the model
type(AnalysisData_tp)  :: AnalysisInfo ! Holds initial data required for array allocation
type(model_tp)         :: Discretization ! Holds all information required for discretization
type(SolverWithLimiter):: Experiment_TypeII !  info to solve shallow water equation with limiter

