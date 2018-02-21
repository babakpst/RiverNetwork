
! Integer Variables -------------------------------------------------------------------------------

! Tiny Integers
!Integer (Kind=Tiny)  ::                         ! ---


! Smll Integers
Integer (Kind=Smll) :: UnFile        ! Holds Unit of a file for error message
Integer (Kind=Smll) :: IO_File       ! For IOSTAT: Input Output Status in OPEN command
Integer (Kind=Smll) :: ERR_Alloc, ERR_DeAlloc ! Allocating and DeAllocating errors
Integer (Kind=Smll) :: i_analyses    ! loop index to read the analyses files
Integer (Kind=Smll) :: ii, jj        ! Loop index

! Shrt Integers
!Integer (Kind=Shrt)  ::                         !

! Lng Integers
!Integer (Kind=Lng )  ::                         !

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
!Integer (Kind=Tiny),              Dimension(:)   ::           !

! Smll Integers
!Integer (Kind=Smll), Allocatable, Dimension(:)   ::           !

! Shrt Integers
!Integer (Kind=Shrt), Allocatable, Dimension(:)   ::           !


! Lng Integers
!Integer (Kind=Lng ), Allocatable, Dimension(:)   ::           !
!Integer (Kind=Lng ), Allocatable, Dimension(:,:) ::           !


! - Real array, allocatable -----------------------------------------------------------------------
!Real (Kind=DBL)   , Allocatable, Dimension(:)    ::
!Real (Kind=DBL)   , Allocatable, Dimension(:,:)  ::            !

! - Type ------------------------------------------------------------------------------------------
type(TimeDate_tp)   :: TimeDate   ! Indicates the time and date of simulation
type(Input_Data_tp) :: ModelInfo  ! Holds info. (name, dir, output dir) of the model
type(ArgCommands)   :: Arguments  ! Holds the entered arguments from the command line
type(timing)        :: SimulationTime ! Holds the run time

! - Character Variables ---------------------------------------------------------------------------
!Character (Kind = 1, Len = 50 ) :: arg  ! Holds the entered argument


