
! Integer Variables -------------------------------------------------------------------------------

! Tiny Integers
!integer(Kind=Tiny)  ::                         ! ---


! Smll Integers
integer(Kind=Smll) :: UnFile        ! Holds Unit of a file for error message
integer(Kind=Smll) :: IO_File       ! For IOSTAT: Input Output status in OPEN command
integer(Kind=Smll) :: ERR_Alloc, ERR_DeAlloc ! Allocating and DeAllocating errors
integer(Kind=Smll) :: i_analyses    ! loop index to read the analyses files
integer(Kind=Smll) :: ii, jj        ! Loop index

! Shrt Integers
!integer(Kind=Shrt)  ::                         !

! Lng Integers
!integer(Kind=Lng )  ::                         !

! - Real Variables --------------------------------------------------------------------------------
Real (Kind=DBL)      :: TimeS, TimeE            ! TIME Variables for total run time
Real (Kind=DBL)      :: TimeInputS, TimeInputE  ! TIME Variables for reading input files
Real (Kind=DBL)      :: TimeSolveS, TimeSolveE  ! TIME Variables for Effective Stiffness

! - Logical Variable ------------------------------------------------------------------------------
!Logical (Kind=Shrt)  ::


! - Integer Arrays --------------------------------------------------------------------------------
! - Real Arrays -----------------------------------------------------------------------------------

! - Integer array, allocatable---------------------------------------------------------------------
! Tiny Integers
!integer(Kind=Tiny),              Dimension(:)   ::           !

! Smll Integers
!integer(Kind=Smll), allocatable, Dimension(:)   ::           !

! Shrt Integers
!integer(Kind=Shrt), allocatable, Dimension(:)   ::           !


! Lng Integers
!integer(Kind=Lng ), allocatable, Dimension(:)   ::           !
!integer(Kind=Lng ), allocatable, Dimension(:,:) ::           !


! - Real array, allocatable -----------------------------------------------------------------------
!Real (Kind=DBL)   , allocatable, Dimension(:)    ::
!Real (Kind=DBL)   , allocatable, Dimension(:,:)  ::            !

! - Type ------------------------------------------------------------------------------------------
type(TimeDate_tp)   :: TimeDate   ! Indicates the time and date of simulation
type(Input_Data_tp) :: ModelInfo  ! Holds info. (name, dir, output dir) of the model
type(ArgCommands)   :: Arguments  ! Holds the entered arguments from the command line
type(timing)        :: SimulationTime ! Holds the run time
type(InitialData_tp)   :: InitialInfo ! Holds initial data required for array allocation

! - Character Variables ---------------------------------------------------------------------------
!Character (Kind = 1, Len = 50 ) :: arg  ! Holds the entered argument


