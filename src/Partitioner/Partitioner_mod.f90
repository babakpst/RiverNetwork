
!##################################################################################################
! Purpose: This module writes down the input file for each process.
!
! Developed by: Babak Poursartip
! Supervised by: Clint Dawson
!
! The Institute for Computational Engineering and Sciences (ICES)
! The University of Texas at Austin
!
! ================================ V E R S I O N ==================================================
! V0.00: 04/15/2018 - File initiated.
!
! File version $Id $
!
! Last update: 04/16/2018
!
! ================================ S U B R O U T I N E ============================================
! Partitioner_1D_Sub: Creates the input files for various processes.
!
! ================================ F U N C T I O N ================================================
!
! ================================ L O C A L   V A R I A B L E S ==================================
! (Refer to the main code to see the list of imported variables)
!  . . . . . . . . . . . . . . . . Variables . . . . . . . . . . . . . . . . . . . . . . . . . . .
!
!##################################################################################################

module Partitioner_mod

! Libraries =======================================================================================

! User defined modules ============================================================================
use Parameters_mod
use Discretization_mod

implicit none

contains

!##################################################################################################
! Purpose: This subroutine creates the input file for each partition/process.
!
! Developed by: Babak Poursartip
!
! The Institute for Computational Engineering and Sciences (ICES)
! The University of Texas at Austin
!
! ================================ V E R S I O N ==================================================
! V0.00: 04/15/2018 - Subroutine initiated.
! V0.01: 04/16/2018 - Initiated: Compiled without error for the first time.
!
! File version $Id $
!
! Last update: 04/16/2018
!
! ================================ L O C A L   V A R I A B L E S ==================================
! (Refer to the main code to see the list of imported variables)
!  . . . . . . . . . . . . . . . . Variables . . . . . . . . . . . . . . . . . . . . . . . . . . .
!
!##################################################################################################

subroutine Partitioner_1D_Sub(Geometry, Discretization, ModelInfo)

! Libraries =======================================================================================

! User defined modules ============================================================================

implicit none

! Global variables ================================================================================

! - integer variables -----------------------------------------------------------------------------
!#integer(kind=Shrt), intent(in)    ::
!#integer(kind=Shrt), intent(inout) ::
!#integer(kind=Shrt), intent(out)   ::
! - real variables --------------------------------------------------------------------------------
!#real(kind=Dbl),     intent(in)    ::
!#real(kind=Dbl),     intent(inout) ::
!#real(kind=Dbl),     intent(out)   ::
! - complex variables -----------------------------------------------------------------------------
!#complex,             intent(in)    ::
!#complex,             intent(inout) ::
!#complex,             intent(out)   ::
! - integer Arrays --------------------------------------------------------------------------------
!#integer(kind=Shrt), intent(in),    dimension (:  )  ::
!#integer(kind=Shrt), intent(in),    dimension (:,:)  ::
!#integer(kind=Shrt), intent(in)    ::
!#integer(kind=Shrt), intent(inout) ::
!#integer(kind=Shrt), intent(out)   ::
! - real Arrays -----------------------------------------------------------------------------------
!#real(kind=Dbl),     intent(in),    dimension (:  )  ::
!#real(kind=Dbl),     intent(inout), dimension (:  )  ::
!#real(kind=Dbl),     intent(out),   dimension (:  )  ::
! - character variables ---------------------------------------------------------------------------
!#character(kind = ?, Len = ? ) ::
! - logical variables -----------------------------------------------------------------------------
!#logical   ::
! - types -----------------------------------------------------------------------------------------
type(Input_Data_tp),     intent(In) :: ModelInfo ! Holds info. (name, dir, output dir) of the model
type(Geometry_tp),       intent(In) :: Geometry
type(discretization_tp), intent(In) :: Discretization

! Local variables =================================================================================
! - integer variables -----------------------------------------------------------------------------
integer(kind=Smll) :: UnFile      ! Holds Unit of a file for error message
integer(kind=Smll) :: IO_File        ! For IOSTAT: Input Output Status in OPEN command
integer(kind=Smll) :: IO_write       ! Used for IOSTAT: Input/Output Status in the write command

integer(kind=Shrt) :: i_partition ! Loop index for the number of processes/ranks
integer(kind=Shrt) :: i           ! Loop index
integer(kind=Shrt) :: remainder   !

! - real variables --------------------------------------------------------------------------------
!#real(kind=Dbl)      ::
! - complex variables -----------------------------------------------------------------------------
!#complex              ::
! - integer Arrays --------------------------------------------------------------------------------
integer(kind=Lng), dimension (Geometry%size)  :: chunk       ! share of the domain for each rank

! - real Arrays -----------------------------------------------------------------------------------
!#real(kind=Dbl), dimension (:)      ::
!#real(kind=Dbl), allocatable, dimension (:)  ::
! - character variables ---------------------------------------------------------------------------
Character(kind = 1, len = 20) :: IndexRank ! Rank no in the Char. fmt to add to the input file Name
Character(kind = 1, len = 20) :: IndexSize ! Size no in the Char. fmt to add to the input file Name

! - logical variables -----------------------------------------------------------------------------
!#logical   ::
! - type ------------------------------------------------------------------------------------------
type(vector), dimension(-1_Lng:NCells+2_Lng)  :: U ! The solution, required for initialization

! code ============================================================================================
write(*,       *) " subroutine < Partitioner_1D_Sub >: "
write(FileInfo,*) " subroutine < Partitioner_1D_Sub >: "


! Initialization of the solution
U(1:Discretization%NCells)%U(1) = this%AnalysisInfo%CntrlV -    Discretization%ZCell(:)
U(0 )%U(1) = U(1)%U(1)
U(-1)%U(1) = U(1)%U(1)

U(Discretization%NCells+1)%U(1) = U(Discretization%NCells)%U(1)
U(Discretization%NCells+2)%U(1) = U(Discretization%NCells)%U(1)

U(:)%U(2) = 0.0_Dbl   ! Setting the discharge/velocity equal to zero <modify> read this from the input file

! Computing the chunk of each process
remainder = mod(Discretization%NCells, Geometry%size)
chunk(:) = (Discretization%NCells - remainder)/Geometry%size

  do i = 1_Shrt, remainder
    chunk(i) = chunk(i) + 1
  end do

write(*,       *) " -Data partitioning ... "
write(FileInfo,*) " -Data partitioning ... "

  On_Partitions: do i_partition = 1, Geometry%size
    write(*,        fmt="(A,I10)") " Partitioning for process no.: ", i_partition-1_Shrt
    write(FileInfo, fmt="(A,I10)") " Partitioning for process no.: ", i_partition-1_Shrt

    ! Opening the output files.
    write(*,       *) " Creating input files ..."
    write(FileInfo,*) " Creating input files ..."

    ! Directories for the input files <modify>
    ! open file for this partition
    write (IndexRank, *) i_partition - 1_Shrt ! Converts Rank to Character format for the file Name
    write (IndexSize, *) Geometry%size        ! Converts Size to Character format for the file Name

    UnFile = FilePartition
    open(unit=UnFile, &
    file=trim(ModelInfo%ModelName)// &
              '_s'//Trim(AdjustL(IndexSize))//'_p'//Trim(AdjustL(IndexRank))//'.par', &
         Err=1001, iostat=IO_File, &
         access='sequential', action='write', asynchronous='no', blank='NULL', blocksize=0, &
         defaultfile=trim(ModelInfo%InputDir), dispose='keep', form='formatted', position='asis',&
         status='replace')

    ! Writing the total number of cells in each partition
    UnFile = FilePartition
    write(unit=UnFile, fmt="('I23')",   advance='yes', asynchronous='no', iostat=IO_write, err=1006) chunk(i_partition)
    ! Writing the length of each cell in each partition
    write(unit=UnFile, fmt="('F23.8')", advance='yes', asynchronous='no', iostat=IO_write, err=1006) Discretization%LengthCell(1)

    counter = 0
    UnFile = FilePartition
    do i_cells = 1_Lng, chunk(i_partition)
      counter = counter + 1
      write(unit=UnFile, fmt="('F23.8')", &
            advance='yes', asynchronous='no', iostat=IO_write, err=1006) &
            Discretization%LengthCell(Discretization%NCells),              &
            Discretization%SlopeCell(Discretization%NCells),               &
            Discretization%ZCell(Discretization%NCells),                   &
            Discretization%ManningCell(Discretization%NCells),             &
            Discretization%WidthCell(Discretization%NCells),               &
            Discretization%X_Disc(Discretization%NCells),                  &


    end do











Discretization%SlopeInter(Discretization%NCells+1),            &
Discretization%ZFull(Discretization%NCells*2_Sgl + 1_Sgl),     &
Discretization%X_Full(Discretization%NCells*2_Sgl + 1_Sgl),    &






















    ! - Closing the input file for this partition -------------------------------------------------
    write(*,        *) " Closing the input file ... "
    write(FileInfo, *) " Closing the input file ... "

    UnFile =  FilePartition
    close(unit=UnFile, status="keep", err=1002, iostat=IO_File)

  end do On_Partitions

write(*,       *) " Partitioning conducted successfully."
write(FileInfo,*) " Partitioning conducted successfully."

write(*,       *) " end subroutine < Partitioner_1D_Sub >"
write(FileInfo,*) " end subroutine < Partitioner_1D_Sub >"
return

! Errors ==========================================================================================
! Opening statement Errors
1001 if (IO_File > 0) then
       write(*, Fmt_Err1_OPEN) UnFile, IO_File; write(FileInfo, Fmt_Err1_OPEN) UnFile, IO_File;
       write(*, Fmt_FL); write(FileInfo, Fmt_FL);
       write(*, Fmt_end); read(*,*); stop;
     else if ( IO_File < 0 ) then
       write(*, Fmt_Err1_OPEN) UnFile, IO_File
       write(FileInfo, Fmt_Err1_OPEN) UnFile, IO_File;  write(*, Fmt_FL) ; write(FileInfo, Fmt_FL);
       write(*, Fmt_end); read(*,*); stop;
     end if

! Close statement Errors
1002 if (IO_File > 0) then
       write(*, Fmt_Err1_Close) UnFile, IO_File; write(FileInfo, Fmt_Err1_Close) UnFile, IO_File;
       write(*, Fmt_FL); write(FileInfo, Fmt_FL);
       write(*, Fmt_End); read(*,*); stop;
     end if

! write statement errors
1006 write(*, Fmt_write1) UnFile, IO_write; write(UnFile, Fmt_write1) UnFile, IO_write;
     write(*, Fmt_FL); write(FileInfo, Fmt_FL); write(*, Fmt_End); read(*,*);  stop;


end subroutine Partitioner_1D_Sub

end module Partitioner_mod
