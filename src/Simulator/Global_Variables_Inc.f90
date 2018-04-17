
! Integer Variables -------------------------------------------------------------------------------

! Tiny Integers
!integer(kind=Tiny)  ::                         ! ---

! Smll Integers
integer(kind=Smll) :: UnFile        ! Holds Unit of a file for error message
integer(kind=Smll) :: IO_File       ! For IOSTAT: Input Output status in OPEN command
integer(kind=Smll) :: ERR_Alloc, ERR_DeAlloc ! Allocating and DeAllocating errors
integer(kind=Smll) :: i_analyses    ! loop index to read the analyses files
integer(kind=Smll) :: ii, jj        ! Loop index

! Shrt Integers
!integer(kind=Shrt)  ::                         !

! Lng Integers
!integer(kind=Lng )  ::                         !

! - Real Variables --------------------------------------------------------------------------------
Real (kind=DBL)      :: TimeS, TimeE            ! TIME Variables for total run time
Real (kind=DBL)      :: TimeInputS, TimeInputE  ! TIME Variables for reading input files
Real (kind=DBL)      :: TimeSolveS, TimeSolveE  ! TIME Variables for Effective Stiffness

! - Logical Variable ------------------------------------------------------------------------------
!Logical (kind=Shrt)  ::

! - Integer Arrays --------------------------------------------------------------------------------
! - Real Arrays -----------------------------------------------------------------------------------

! - Integer array, allocatable---------------------------------------------------------------------
! Tiny Integers
!integer(kind=Tiny),              Dimension(:)   ::           !

! Smll Integers
!integer(kind=Smll), allocatable, Dimension(:)   ::           !

! Shrt Integers
!integer(kind=Shrt), allocatable, Dimension(:)   ::           !


! Lng Integers
!integer(kind=Lng ), allocatable, Dimension(:)   ::           !
!integer(kind=Lng ), allocatable, Dimension(:,:) ::           !


! - Real array, allocatable -----------------------------------------------------------------------
!Real (kind=DBL)   , allocatable, Dimension(:)    ::
!Real (kind=DBL)   , allocatable, Dimension(:,:)  ::            !

! - Type ------------------------------------------------------------------------------------------
type(TimeDate_tp)   :: TimeDate   ! Indicates the time and date of simulation
type(ArgCommands)   :: Arguments  ! Holds the entered arguments from the command line
type(timing)        :: SimulationTime ! Holds the run time
type(AnalysisData_tp):: AnalysisInfo ! Holds initial data required for array allocation
type(discretization_tp):: Discretization ! Holds all information required for discretization
type(SolverWithLimiter(NCells=:)), allocatable :: Experiment_TypeII ! Contains all info to solve shallow water equation using Richtmyer method


! - Character Variables ---------------------------------------------------------------------------
!Character (kind = 1, Len = 50 ) :: arg  ! Holds the entered argument
