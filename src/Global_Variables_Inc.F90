
! Integer Variables -------------------------------------------------------------------------------

! Tiny Integers
Integer (Kind=Tiny)  :: ;                   ! ---


! Smll Integers
Integer (Kind=Smll)  :: Year, Month, Day ;               ! Date variables
Integer (Kind=Smll)  :: Hour, Minute, Seconds, S100th ;  ! Time variables
Integer (Kind=Smll)  :: IO_File ;                ! For IOSTAT: Input Output Status in OPEN cammand
Integer (Kind=Smll)  :: ERR_Alloc, ERR_DeAlloc ; ! Allocating and DeAllocating errors
Integer (Kind=Smll)  :: UnFile ;                 ! Holds Unit of a file for error message

! Shrt Integers
Integer (Kind=Shrt)  :: ;                        !

! Lng Integers
Integer (Kind=Lng )  :: ;                        !


! - Real Variables ----------------------------------------------------------------------------------------------------------------------------------
Real (Kind=DBL)      :: TimeS, TimeE ;           ! TIME Variables for total runtime
Real (Kind=DBL)      :: TimeInputS, TimeInputE ; ! TIME Variables for reading input files
Real (Kind=DBL)      :: TimeAssemS, TimeAssemE ; ! TIME Variables for forming global matrices
Real (Kind=DBL)      :: TimeIndexS, TimeIndexE ; ! TIME Variables for object definition and Indexing
Real (Kind=DBL)      :: TimeEStiffS, TimeEStiffE;! TIME Variables for Effective Stiffness
Real (Kind=DBL)      :: TimeSolveS, TimeSolveE ; ! TIME Variables for Effective Stiffness

! - Logical Variable --------------------------------------------------------------------------------------------------------------------------------
Logical (Kind=Shrt)  :: Directory ;

! - Integer Arrays ----------------------------------------------------------------------------------------------------------------------------------
! - Real Arrays -------------------------------------------------------------------------------------------------------------------------------------

! - Integer ARRAY ALLOCATION ------------------------------------------------------------------------------------------------------------------------
! Tiny Integers
Integer (Kind=Tiny),              Dimension(6)   :: ;          !

! Smll Integers
Integer (Kind=Smll), Allocatable, Dimension(:)   :: ;          !

! Shrt Integers
Integer (Kind=Shrt), Allocatable, Dimension(:)   :: ;          !


! Lng Integers
Integer (Kind=Lng ), Allocatable, Dimension(:)   :: ;          !
Integer (Kind=Lng ), Allocatable, Dimension(:,:) :: ;          !


! - Real ARRAY ALLOCATION ---------------------------------------------------------------------------------------------------------------------------
Real (Kind=DBL)   , Allocatable, Dimension(:)    :: ;

Real (Kind=DBL)   , Allocatable, Dimension(:,:)  :: ;           !

! - Type ALLOCATION ---------------------------------------------------------------------------------------------------------------------------------
Type ( .. )    :: ;                !

! - Character Variables -----------------------------------------------------------------------------------------------------------------------------
Character (Kind = 1, Len = 30 ) :: ;   !


