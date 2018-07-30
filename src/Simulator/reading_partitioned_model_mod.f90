
!##################################################################################################
! Purpose: This module reads all data for simulation.
!
! Developed by: Babak Poursartip
! Supervised by: Clint Dawson
!
! The Institute for Computational Engineering and Sciences (ICES)
! The University of Texas at Austin
!
! ================================ V E R S I O N ==================================================
! V0.10: 02/22/2018 - Initiation.
! V0.10: 03/08/2018 - Initiated: Compiled without error.
! V1.00: 04/20/2018 - Major modifications
! V2.00: 05/15/2018 - Separating model from the input
! V3.00: 07/30/2018 - Modifying the input subroutine to read a partitioned network
!
! File version $Id $
!
! Last update: 07/30/2018
!
! ================================ S U B R O U T I N E ============================================
! reading_partitioned_network
!
! ================================ F U N C T I O N ================================================
!
! ================================ L O C A L   V A R I A B L E S ==================================
! (Refer to the main code to see the list of imported variables)
!  . . . . . . . . . . . . . . . . Variables . . . . . . . . . . . . . . . . . . . . . . . . . . .
!
!##################################################################################################

module reading_network_mod

use Parameters_mod
use Input_mod
use messages_and_errors_mod

implicit none
private

! contains all information about individual reaches after discretization and partitioning
type DiscretizedReach_tp
  integer (kind=Lng)  :: NCells_reach = 0_Lng ! number of cells in the reach
  integer (kind=Lng)  :: ReachNumber  = 0_Lng ! The reach no. in the unpartitioned network

  integer (kind=Shrt) :: Communication= 0_Lng ! if this reach communicates with other ranks
                                              ! -1: the entire rank is on one rank-no communication
                                              ! 1: indicates that we need to communicate with the
                                              !    rank that holds the lower part of the rank.
                                              ! 2: indicates that we need to communicate with the
                                              !    rank that holds the upper part of the rank.

  integer(kind=Shrt)  :: CommRank =-1_Shrt    ! indicates the rank number that a reach needs to
                                              ! communicate with, i.e., if the reach is divided
                                              ! between two ranks.

  integer(kind=tiny) :: BCNodeI   = -1_Shrt   ! the BC of the upstream node of the reach,
  integer(kind=tiny) :: BCNodeII  = -1_Shrt   ! the BC of the downstream node of the reach,
                                              ! -1 not on this rank, 0 BC,
                                              !  1 connected to other nodes, 2 outlet

  real(kind=DBL) :: ReachManning         = 0.0_dbl ! the Manning's number of each cell
  real(kind=DBL) :: ReachWidthCell       = 0.0_dbl ! the Manning's number of each cell
  real(kind=DBL) :: CellPorjectionLength = 0.0_dbl ! the length of each cell in the horizontal dir.

  real(kind=DBL), allocatable, dimension(:) :: LengthCell     ! the length of each cell
  real(kind=DBL), allocatable, dimension(:) :: CellSlope      ! slope of each cell at the center
  real(kind=DBL), allocatable, dimension(:) :: InterfaceSlope ! slope of each cell at the center

  ! coordinate
  real(kind=DBL), allocatable, dimension(:) :: ZCell     ! bottom elev. at the center of each cell
  real(kind=DBL), allocatable, dimension(:) :: YCell     ! the coordinates of the cell center
  real(kind=DBL), allocatable, dimension(:) :: XCell     ! the coordinates of the cell center

  !real(kind=DBL), allocatable, dimension(:) :: ZFull    ! bottom elevation at cells and interfaces
  !real(kind=DBL), allocatable, dimension(:) :: YFull    ! coordinates at cells and interfaces
  !real(kind=DBL), allocatable, dimension(:) :: XFull    ! coordinates at cells and interfaces
end type DiscretizedReach_tp


! contains the information about the entire network, separated by reaches
type network_tp

  ! Total number of cells from the network on this rank
  integer(kind=Lng) :: TotalNumOfCellsOnThisRank = 0_Lng

  ! Total number of ranks from the network on this rank
  integer(kind=Lng) :: TotalNumOfReachesOnThisRank = 0_Lng

  ! To hold the discretization of each reach. The size of this type is equal to the no of reaches.
  type(DiscretizedReach_tp), allocatable, dimension(:) :: DiscretizedReach

  contains
    procedure read => reading_partitioned_network
end type network_tp

public:: network_tp

contains

!##################################################################################################
! Purpose: This subroutine reads the partitioned data created by the partitioner. All the types and
! the variables are based on the partitioner code.
!
! Developed by: Babak Poursartip
! Supervised by: Clint Dawson
!
! The Institute for Computational Engineering and Sciences (ICES)
! The University of Texas at Austin
!
! ================================ V E R S I O N ==================================================
! V0.1: 02/23/2018 - Initiation.
! V1.0: 03/01/2018 - Compiled with no errors/warnings.
! V1.0: 04/10/2018 - Minor modifications in the class.
! V2.0: 04/20/2018 - Parallel.
! V2.1: 05/24/2018 - Parallel with MPI
! V3.0: 07/30/2018 - reading the partitioned network
!
! File version $Id $
!
! Last update: 07/30/2018
!
! ================================ L O C A L   V A R I A B L E S ==================================
! (Refer to the main code to see the list of imported variables)
!  . . . . . . . . . . . . . . . . Variables . . . . . . . . . . . . . . . . . . . . . . . . . . .
!
!##################################################################################################

Subroutine reading_partitioned_network(this, ModelInfo)

! Libraries =======================================================================================

! User defined modules ============================================================================

Implicit None

! Global Variables ================================================================================

! - types -----------------------------------------------------------------------------------------
type(Input_Data_tp), intent(In) :: ModelInfo  ! Holds info. (name, dir, output dir) of the model
class(model_tp),  intent(inout) :: this       ! Holds the entire model

! Local Variables =================================================================================
! - integer Variables -----------------------------------------------------------------------------
integer(kind=Smll) :: ERR_Alloc, ERR_DeAlloc ! Allocating and DeAllocating errors
integer(kind=Smll) :: UnFile                 ! Holds Unit of a file for error message
integer(kind=Smll) :: IO_File                ! For IOSTAT: Input Output status in OPEN command
integer(kind=Smll) :: IO_read                ! Holds error of read statements
integer(kind=Smll) :: IO_write               ! Used for IOSTAT in the write statement:In/Out Status
integer(kind=Smll) :: i_reach                ! loop index on the number of reaches
integer(kind=Lng)  :: i_cells                ! loop index on the number of reaches

! code ============================================================================================
write(*,       *)
write(*,       *) " Subroutine < Input_sub >: "
write(FileInfo,*)
write(FileInfo,*) " Subroutine < Input_sub >: "

! Open required Files -----------------------------------------------------------------------------
write(*,        fmt="(A)") " -Opening the input file ..."

write(FileInfo, fmt="(A)") " -Opening the input file ..."

! Open the input file for arrays
UnFile = FilePartition
open(Unit=UnFile, file=trim(ModelInfo%ModelNameParallel)//'.par', &
     err=1001, iostat=IO_File, access='sequential', action='read', asynchronous='no', &
     blank='null', blocksize=0, defaultfile=trim(ModelInfo%InputDir), DisPOSE='keep', &
     form='formatted', position='asis', status='old')







! Up to here

UnFile = FilePartition
read(unit=UnFile, fmt="(I23)", advance='yes', asynchronous='no', iostat=IO_read, err=1003, &
     end=1004) this%NCells

write(*,        fmt="(A)") " -Allocating discretization ..."
write(FileInfo, fmt="(A)") " -Allocating discretization ..."

allocate(this%LengthCell(this%NCells,2),            &
         this%CellSlope(this%NCells),               &
         this%InterfaceSlope(this%NCells+1),        &
         this%ZCell(this%NCells),                   &
         this%ManningCell(this%NCells),             &
         this%WidthCell(this%NCells),               &
         this%XCell(this%NCells),                   &
         stat=ERR_Alloc)
         !this%XFull(this%NCells*2_Lng + 1_Lng),    &
         !this%ZFull(this%NCells*2_Lng + 1_Lng),     &
if (ERR_Alloc /= 0) call error_in_allocation(ERR_Alloc)

UnFile = FilePartition
  do i_cells = 1_Lng, this%NCells
    read(unit=UnFile, fmt="(8F35.20)", advance='yes', asynchronous='no', iostat=IO_read, &
    err=1003, end=1004) &
    this%LengthCell (i_cells,1),                       &
    this%LengthCell (i_cells,2),                       &
    this%CellSlope  (i_cells),                         &
    this%ZCell      (i_cells),                         &
    this%ManningCell(i_cells),                         &
    this%WidthCell  (i_cells),                         &
    this%XCell     (i_cells),                          &
    this%InterfaceSlope (i_cells)
  end do
!read(unit=UnFile, fmt="(F35.20)", advance='yes', asynchronous='no', iostat=IO_read, &
!    err=1003, end=1004) this%InterfaceSlope(i_cells)

read(unit=UnFile, fmt="(F35.20)", advance='yes', asynchronous='no', iostat=IO_read, &
    err=1003, end=1004) this%InterfaceSlope(this%NCells+1)

! - Closing the input file ------------------------------------------------------------------------
write(*,        fmt="(A)") " -Closing the input file"
write(FileInfo, fmt="(A)") " -Closing the input file"

UnFile = FilePartition
close(Unit = UnFile, status = 'keep', ERR =  1002, IOSTAT = IO_File)

write(*,       *) " End Subroutine < Input_sub >"
write(*,       *)
write(FileInfo,*) " End Subroutine < Input_sub >"
write(FileInfo,*)
Return

! Errors ==========================================================================================
! Opening statement Errors
1001 call errorMessage(UnFile, IO_File)

! Close statement Errors
1002 call error_in_closing_a_file(UnFile, IO_File)

! - Error in read statement -----------------------------------------------------------------------
1003 call error_in_reading_data_from_file(UnFile, IO_read)

! - End-OF-FILE in read statement -----------------------------------------------------------------
1004 call error_in_reading_data_from_file_EOF(UnFile, IO_read)

! - write statement error -------------------------------------------------------------------------
1006 call error_in_writing(UnFile, IO_write)

end Subroutine reading_partitioned_network

end module reading_network_mod
