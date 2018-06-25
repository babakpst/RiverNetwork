
! Integer Variables -------------------------------------------------------------------------------

! Smll Integers
integer(kind=Smll) :: UnFile        ! Holds Unit of a file for error message
integer(kind=Smll) :: IO_File       ! For IOSTAT: Input Output status in OPEN command
integer(kind=Smll) :: ERR_Alloc, ERR_DeAlloc ! Allocating and DeAllocating errors
integer(kind=Smll) :: ii, jj        ! Loop index

! - Real Variables --------------------------------------------------------------------------------
Real (kind=DBL)      :: TimeS, TimeE            ! TIME Variables for total run time
Real (kind=DBL)      :: TimeInputS, TimeInputE  ! TIME Variables for reading input files
Real (kind=DBL)      :: TimeSolveS, TimeSolveE  ! TIME Variables for Effective Stiffness


! - Type ------------------------------------------------------------------------------------------
type(TimeDate_tp)   :: TimeDate       ! Indicates the time and date of simulation
type(Input_Data_tp) :: ModelInfo      ! Holds info. (name, dir, output dir) of the model
type(ArgCommands)   :: Arguments      ! Holds the entered arguments from the command line
type(timing_tp)     :: SimulationTime ! Holds the run time
type(Geometry_tp)   :: Geometry       ! Holds information about the geometry of the domain
type(DiscretizedNetwork_tp) :: Discretization ! Holds the discretized network
type(partitioner_tp):: NetworkPartitioner ! This class handles the network partitioner.

! - Character Variables ---------------------------------------------------------------------------
!Character (kind = 1, Len = 50 ) :: arg  ! Holds the entered argument
