
!##################################################################################################
! Purpose: Reads file name and directories from the address file.
!
! Developed by:  Babak Poursartip
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
! V2.01: 07/24/2018 - Modifications on the style
!
! File version $Id $
!
! Last update: 07/24/2018
!
! ================================ S U B R O U T I N E ============================================
! Input_Address_sub: Reads file name and directories from the address file.
! Input_Analysis_sub
!
! ================================ F U N C T I O N ================================================
!
! ================================ L O C A L   V A R I A B L E S ==================================
! (Refer to the main code to see the list of imported variables)
!  . . . . . . . . . . . . . . . . Variables . . . . . . . . . . . . . . . . . . . . . . . . . . .
!
!##################################################################################################

module Input_mod

use Parameters_mod
use messages_and_errors_mod

implicit none
private

! Holds info. (name, dir. output dir.) of the model, initialized by subroutine Input_Address_sub.
type Input_Data_tp
  character (kind = 1, Len = 150) :: ModelName     ! Name of the model input file
  character (kind = 1, Len = 150) :: ModelNameParallel ! Name of the model input file with info
                                                      ! about Parallel simulation
  character (kind = 1, Len = 150) :: InputDir      ! Directory of the input file.
  character (kind = 1, Len = 150) :: AnalysisDir   ! Directory of Analysis input file.
  character (kind = 1, Len = 150) :: OutputDir     ! Directory of output files (Results)
  character (kind = 1, Len = 150) :: AnalysisOutputDir ! Dir of output file for each analysis
  character (kind = 1, Len = 150) :: VisualizerDir ! Dir of output file for visualizer(python)

  ! Holds the names of the analysis input files
  character (kind = 1, Len = 150), dimension(:), allocatable :: AnalysesNames

  ! rank and size of MPI
  Character(kind = 1, len = 20) :: IndexRank !Rank no in the Char. fmt to add to input file Name
  Character(kind = 1, len = 20) :: IndexSize !Size no in the Char. fmt to add to input file Name

  !integer(kind=Smll):: OutputType                ! Output Type: 1: ordinary-2: HDF5
  integer(kind=Smll) :: NumberOfAnalyses          ! Number of analysis
  integer            :: size, rank                ! Size and rank of Parallel MPI

  real(kind=SGL) :: Version               ! Holds the version of the code.

  contains
    procedure :: Input      => Input_Address_sub
    procedure :: visualizer => Python_Visualizer_sub
end type Input_Data_tp

! Contains all information about the domain, required
type AnalysisData_tp
  integer(kind=Smll):: AnalysisType=2_smll! Analysis Type -1:1D Lax-Wendroff
                                          !               -2:1D Lax-Wendroff with limiter
  integer(kind=Smll):: limiter  = 1_smll  ! limiter type

  integer(kind=Lng) :: Plot_Inc = 500_Lng ! Increment to record the results for visualization

  real(kind=DBL):: TotalTime    = 0.0_dbl ! Total simulation time (in seconds)
  real(kind=DBL):: TimeStep     = 0.0_dbl ! Time Step
  real(kind=DBL):: Q_Up         = 0.0_dbl ! Upstream boundary condition, constant flow (m^3/s)
  real(kind=DBL):: h_dw         = 0.0_dbl ! Downstream water depth (in meters)
  real(kind=DBL):: CntrlV       = 0.0_dbl ! Initial control volume
  real(kind=DBL):: CntrlV_ratio = 0.0_dbl ! Initial control volume ration, used to initialize data

  contains
    procedure:: Analysis => Input_Analysis_sub
end type AnalysisData_tp

public:: AnalysisData_tp, Input_Data_tp

contains

!##################################################################################################
! Purpose: This subroutine/function reads the general information about the name of the simulation
!           model, directories, etc.
!
! Developed by: Babak Poursartip
! Supervised by:
!
! The Institute for Computational Engineering and Sciences (ICES)
! The University of Texas at Austin
!
! ================================ V E R S I O N ==================================================
! V0.1: 02/21/2018 - Initiation.
! V1.0: 03/01/2018 - Compiled without error.
! V1.1: 04/10/2018 - Minor modifications.
!
! File version $Id $
!
! Last update: 04/10/2018
!
! ================================ L O C A L   V A R I A B L E S ==================================
! (Refer to the main code to see the list of imported variables)
!  . . . . . . . . . . . . . . . . Variables . . . . . . . . . . . . . . . . . . . . . . . . . . .
!
!##################################################################################################

Subroutine Input_Address_sub(this)

! Libraries =======================================================================================
use ifport


! User defined modules ============================================================================

Implicit None

! Global Variables ================================================================================
! - Types -----------------------------------------------------------------------------------------
class(Input_Data_tp), intent(out) :: this  ! Holds info. (name, dir, output dir) of the model

! Local Variables =================================================================================
! - integer Variables -----------------------------------------------------------------------------
integer(kind=Smll) :: UnFile        ! Holds Unit of a file for error message
integer(kind=Smll) :: IO_File       ! For IOSTAT: Input Output status in OPEN command
integer(kind=Smll) :: i_analyses    ! loop index to read the analyses files
integer(kind=Smll) :: ERR_Alloc, ERR_DeAlloc ! Allocating and DeAllocating errors

! - Logical Variables -----------------------------------------------------------------------------
Logical (kind=Shrt)  :: Directory

! code ============================================================================================

write(*,       *)
write(*,       *) " Subroutine < Input_Address_sub >: "
!write(FileInfo,*)
!write(FileInfo,*) " Subroutine < Input_Address_sub >: "

UnFile=FileAdr
Open(Unit=UnFile, File='Address.txt', Err=1001, IOStat=IO_File, Access='SEQUENTIAL', &
     action='READ', Asynchronous='NO', blank='NULL', blocksize=0, DisPOSE='Keep', &
     Form='formatted', position='ASIS', status='old')

! Read the input fine name and directories Form "ADDRESS_File.txt" in the current directory -------
read(FileAdr,*)
read(FileAdr,*) this%ModelName; !write(*,*) this%ModelName
read(FileAdr,*)
read(FileAdr,*)
read(FileAdr,*) this%InputDir;  !write(*,*) this%InputDir
read(FileAdr,*)
read(FileAdr,*)
read(FileAdr,*) this%OutputDir; !write(*,*) this%OutputDir
 read(FileAdr,*)
read(FileAdr,*)
read(FileAdr,*) this%NumberOfAnalyses; !write(*,*) this%NumberOfAnalyses

! Allocating
allocate(this%AnalysesNames(this%NumberOfAnalyses),  stat=ERR_Alloc)
  if (ERR_Alloc /= 0) call error_in_allocation(ERR_Alloc)

read(FileAdr,*)
read(FileAdr,*)
  do i_analyses = 1, this%NumberOfAnalyses
    read(FileAdr,*) this%AnalysesNames(i_analyses)
  end do


! Comment: we read the rank and size directly in the main code, by calling MIP routines
write(this%IndexRank, *) this%rank ! Converts Rank to Character format for the file Name
write(this%IndexSize, *) this%size ! Converts Size to Character format for the file Name

this%AnalysisDir=trim(AdjustL(this%InputDir))//'/'// &
                      trim(AdjustL(this%ModelName))//'/'//'Analysis'
this%InputDir   =trim(AdjustL(this%InputDir))//'/'// &
                      trim(AdjustL(this%ModelName))//'/'//'Model'

write(*, fmt="(2A)")" The model directory is: ", this%InputDir
write(*, fmt="(2A)")" The analysis name is: ",   this%AnalysisDir
write(*, fmt="(2A)")" The input file name is: ", this%ModelName

! Create the results folder
write(*,fmt="(A)") " -Creating the output folders ..."

Directory=MakeDirQQ (trim(AdjustL(this%OutputDir))//'/'//trim(AdjustL(this%ModelName)))
  if (Directory) then
    write(*,fmt="(A)") " The result folder is created."
  Else
     write(*,fmt="(A)") " The result folder already exists."
  end if

this%OutputDir=trim(AdjustL(this%OutputDir))//'/'//trim(AdjustL (this%ModelName))

write(*,fmt="(2A)")" The output directory is: ", this%OutputDir

! Modifying the model name for parallel simulation
this%ModelNameParallel = trim(AdjustL(this%ModelName))//'_s'// &
                         trim(adjustL(this%IndexSize))//'_p'//trim(adjustL(this%IndexRank))

write(*,fmt='(" The name of the model file is: ",2A)') this%ModelNameParallel


this%VisualizerDir = "../../visualizer"

! - Closing the address file ----------------------------------------------------------------------
write(*,        fmt="(A)") " -Closing the address file"
!write(FileInfo, fmt="(A)") " -Closing the address file"
UnFile =  FileAdr
Close(unit = UnFile, status = 'keep', ERR =  1002, IOSTAT = IO_File)


write(*,       *) ' End Subroutine < Input_Address_sub >'
write(*,       *)
!write(FileInfo,*) ' End Subroutine < Input_Address_sub >'
!write(FileInfo,*)
Return

! Errors ==========================================================================================
! Opening statement Errors
1001 call errorMessage(UnFile, IO_File)

! Close statement Errors
1002 call error_in_closing_a_file(UnFile, IO_File)

End Subroutine Input_Address_sub


!##################################################################################################
! Purpose: This subroutine reads information required for a particular analysis.
!
! Developed by: Babak Poursartip
! Supervised by: Clint Dawson
!
! The Institute for Computational Engineering and Sciences (ICES)
! The University of Texas at Austin
!
! ================================ V E R S I O N ==================================================
! V0.1: 02/13/2018 - Initiation.
! V1.0: 03/01/2018 - Compiled with no errors/warnings for the first time.
! V1.0: 04/10/2018 - Minor modifications in the classes/objects.
!
! File version $Id $
!
! Last update: 04/01/2018
!
! ================================ L O C A L   V A R I A B L E S ==================================
! (Refer to the main code to see the list of imported variables)
!  . . . . . . . . . . . . . . . . Variables . . . . . . . . . . . . . . . . . . . . . . . . . . .
!
!##################################################################################################

Subroutine Input_Analysis_sub(this, i_analyses, ModelInfo)

! Libraries =======================================================================================
use ifport

! User defined modules ============================================================================
use messages_and_errors_mod

Implicit None

! Global Variables ================================================================================

! - integer Variables -----------------------------------------------------------------------------
integer(kind=Smll), intent(In) :: i_analyses

! - Types -----------------------------------------------------------------------------------------
type(Input_Data_tp), intent(inout) :: ModelInfo  ! Holds info. (name, dir, output dir) of the model
class(AnalysisData_tp), intent(out) :: this      ! Holds analysis information

! Local Variables =================================================================================
! - integer Variables -----------------------------------------------------------------------------
integer(kind=Smll) :: UnFile        ! Holds Unit of a file for error message
integer(kind=Smll) :: IO_File       ! For IOSTAT: Input Output status in OPEN command
integer(kind=Smll) :: IO_read  ! Holds error of read statements
integer(kind=Smll) :: IO_write ! Used for IOSTAT - Input Output Status - in the write command.

! - Logical Variables -----------------------------------------------------------------------------
logical(kind=Shrt)  :: Directory

! code ============================================================================================
write(*,       *)
write(*,       *) " Subroutine < Input_Analysis_sub >: "
write(FileInfo,*)
write(FileInfo,*) " Subroutine < Input_Analysis_sub >: "


! Opening the input file for this specific simulation
write(*,        fmt="(A)") " -Opening the analysis file ..."
write(FileInfo, fmt="(A)") " -Opening the analysis file ..."

UnFile=UnInptAna
Open(Unit=UnFile, File=trim(ModelInfo%AnalysesNames(i_analyses))//'.Analysis', Err= 1001, &
      IOStat=IO_File, Access='sequential', action='read', Asynchronous='no', blank='null', &
      blocksize=0, defaultfile=trim(ModelInfo%AnalysisDir), dispose='Keep', form='formatted', &
      position='asis', status='old') ;

! Creating the output file directory for this analysis --------------------------------------------
write(*,        fmt="(A)") " -Creating the output folder for this analysis ..."
write(FileInfo, fmt="(A)") " -Creating the output folder for this analysis ..."

Directory=MakeDirQQ(trim(AdjustL(ModelInfo%OutputDir))//'/'// &
                    trim(AdjustL(ModelInfo%AnalysesNames(i_analyses)))//'_s'// &
                    trim(AdjustL(ModelInfo%IndexSize)))
  if (Directory) then ;
     write(*,       fmt="(A)") "The output folder for this analysis created." ;
     write(FileInfo,fmt="(A)") "The output folder for this analysis created." ;
  Else ;
     write(*,        fmt="(A)") "The output folder for this analysis already exists." ;
     write(FileInfo, fmt="(A)") "The output folder for this analysis already exists." ;
  end if ;

ModelInfo%AnalysisOutputDir=trim(AdjustL(ModelInfo%OutputDir))//'/'//&
                            trim(AdjustL(ModelInfo%AnalysesNames(i_analyses)))//'_s'// &
                            trim(AdjustL(ModelInfo%IndexSize))

UnFile = UnInptAna
read(unit=UnFile, fmt="(A)", advance='yes', asynchronous='no', iostat=IO_read, err=1003, end=1004)
read(unit=UnFile, fmt="(A)", advance='yes', asynchronous='no', iostat=IO_read, err=1003, end=1004)
read(unit=UnFile, fmt="(A)", advance='yes', asynchronous='no', iostat=IO_read, err=1003, end=1004)
read(unit=UnFile,fmt="(I10)",advance='yes', asynchronous='no', iostat=IO_read, err=1003, end=1004)&
                                                                                  this%AnalysisType

UnFile = FileInfo
write(unit=*,      fmt="(' The Analysis type is: ', I10)") this%AnalysisType
write(unit=UnFile, fmt="(' The Analysis type is: ', I10)", advance='yes', asynchronous='no', &
                   iostat=IO_write, err=1006) this%AnalysisType

UnFile = UnInptAna
read(unit=UnFile, fmt="(A)", advance='yes', asynchronous='no', iostat=IO_read, err=1003, end=1004)
read(unit=UnFile, fmt="(A)", advance='yes', asynchronous='no', iostat=IO_read, err=1003, end=1004)
read(unit=UnFile, fmt="(F23.10)", advance='yes', asynchronous='no', iostat=IO_read, err=1003, &
                  end=1004) this%TotalTime

UnFile = FileInfo
write(unit=*,      fmt="(' The total simulation time is: ', F23.10, ' s')") this%TotalTime
write(unit=UnFile, fmt="(' The total simulation time is: ', F23.10, ' s')", advance='yes', &
                   asynchronous='no', iostat=IO_write, err=1006) this%TotalTime

UnFile = UnInptAna
read(unit=UnFile, fmt="(A)", advance='yes', asynchronous='no', iostat=IO_read, err=1003, end=1004)
read(unit=UnFile, fmt="(A)", advance='yes', asynchronous='no', iostat=IO_read, err=1003, end=1004)
read(unit=UnFile, fmt="(F23.10)", advance='yes', asynchronous='no', iostat=IO_read, &
                  err=1003, end=1004) this%TimeStep

UnFile = FileInfo
write(unit=UnFile, fmt="(' The time step is: ', F23.10, ' s')", advance='yes', asynchronous='no', &
                   iostat=IO_write, err=1006) this%TimeStep
write(unit=*,      fmt="(' The time step is: ', F23.10, ' s')") this%TimeStep

UnFile = UnInptAna
read(unit=UnFile, fmt="(A)", advance='yes', asynchronous='no', iostat=IO_read, err=1003, end=1004)
read(unit=UnFile, fmt="(A)", advance='yes', asynchronous='no', iostat=IO_read, err=1003, end=1004)
read(unit=UnFile, fmt="(F23.10)", advance='yes', asynchronous='no', iostat=IO_read, err=1003, &
                  end=1004) this%Q_Up
UnFile = FileInfo
write(unit=UnFile, fmt="(' Flow rate at the upstream is: ', F23.10, ' m/s3')", advance='yes', &
                   asynchronous='no', iostat=IO_write, err=1006) this%Q_Up
write(unit=*,      fmt="(' Flow rate at the upstream is: ', F23.10, ' m/s3')") this%Q_Up

UnFile = UnInptAna
read(unit=UnFile, fmt="(A)", advance='yes', asynchronous='no', iostat=IO_read, err=1003, end=1004)
read(unit=UnFile, fmt="(A)", advance='yes', asynchronous='no', iostat=IO_read, err=1003, end=1004)
read(unit=UnFile, fmt="(F23.10)", advance='yes', asynchronous='no', iostat=IO_read, &
                  err=1003, end=1004) this%h_dw
UnFile = FileInfo
write(unit=UnFile, fmt="(' Downstream water depth is: ', F23.10, ' m')", advance='yes', &
                   asynchronous='no', iostat=IO_write, err=1006) this%h_dw
write(unit=*,      fmt="(' Downstream water depth is: ', F23.10, ' m')") this%h_dw

UnFile = UnInptAna
read(unit=UnFile, fmt="(A)", advance='yes', asynchronous='no', iostat=IO_read, err=1003, end=1004)
read(unit=UnFile, fmt="(A)", advance='yes', asynchronous='no', iostat=IO_read, err=1003, end=1004)
read(unit=UnFile, fmt="(F23.10)", advance='yes', asynchronous='no', iostat=IO_read, &
                  err=1003, end=1004) this%CntrlV

UnFile = FileInfo
write(unit=UnFile, fmt="(' Control Volume is: ', F23.10, ' m^3')", advance='yes', &
                   asynchronous='no', iostat=IO_write, err=1006) this%CntrlV
write(unit=*,      fmt="(' Control Volume is: ', F23.10, ' m^3')") this%CntrlV

UnFile = UnInptAna
read(unit=UnFile, fmt="(A)", advance='yes', asynchronous='no', iostat=IO_read, err=1003, end=1004)
read(unit=UnFile, fmt="(A)", advance='yes', asynchronous='no', iostat=IO_read, err=1003, end=1004)
read(unit=UnFile, fmt="(F23.10)", advance='yes', asynchronous='no', iostat=IO_read, &
                  err=1003, end=1004) this%CntrlV_ratio

UnFile = FileInfo
write(unit=UnFile, fmt="(' The ratio of control volume is: ', F23.10)", advance='yes', &
                   asynchronous='no', iostat=IO_write, err=1006) this%CntrlV_ratio
write(unit=*,      fmt="(' The ratio of control volume is: ', F23.10)") this%CntrlV_ratio

UnFile = UnInptAna
read(unit=UnFile, fmt="(A)", advance='yes', asynchronous='no', iostat=IO_read, err=1003, end=1004)
read(unit=UnFile, fmt="(A)", advance='yes', asynchronous='no', iostat=IO_read, err=1003, end=1004)
read(unit=UnFile, fmt="(I10)", advance='yes', asynchronous='no', iostat=IO_read, &
                  err=1003, end=1004) this%limiter

UnFile = FileInfo
write(unit=UnFile, fmt="(' The limiter is: ', I10)", advance='yes', asynchronous='no', &
                   iostat=IO_write, err=1006) this%limiter
write(unit=*,      fmt="(' The limiter is: ', I10)") this%limiter


UnFile = UnInptAna
read(unit=UnFile, fmt="(A)", advance='yes', asynchronous='no', iostat=IO_read, err=1003, end=1004)
read(unit=UnFile, fmt="(A)", advance='yes', asynchronous='no', iostat=IO_read, err=1003, end=1004)
read(unit=UnFile, fmt="(I10)", advance='yes', asynchronous='no', iostat=IO_read, &
                  err=1003, end=1004) this%Plot_Inc

UnFile = FileInfo
write(unit=UnFile, fmt="(' The limiter is: ', I10)", advance='yes', asynchronous='no', &
                   iostat=IO_write, err=1006) this%Plot_Inc
write(unit=*,      fmt="(' The limiter is: ', I10)") this%Plot_Inc

write(*,       *) " End Subroutine < Input_Analysis_sub >"
write(*,       *)
write(FileInfo,*) " End Subroutine < Input_Analysis_sub >"
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

End Subroutine Input_Analysis_sub

!##################################################################################################
! Purpose: This subroutine provides the necessary information for the python script to visualize
!          the output of the parallel code.
!
! Developed by: Babak Poursartip
! Supervised by: Clint Dawson
!
! The Institute for Computational Engineering and Sciences (ICES)
! The University of Texas at Austin
!
! ================================ V E R S I O N ==================================================
! V0.00: 05/31/2018 - File initiated.
!
! File version $Id $
!
! Last update: 05/31/2018
!
! ================================ L O C A L   V A R I A B L E S ==================================
! (Refer to the main code to see the list of imported variables)
!  . . . . . . . . . . . . . . . . Variables . . . . . . . . . . . . . . . . . . . . . . . . . . .
!
!##################################################################################################


subroutine Python_Visualizer_sub(this, AnalysisInfo, i_analyses)

! Libraries =======================================================================================


! User defined modules ============================================================================

implicit none

! Global variables ================================================================================

! - integer variables -----------------------------------------------------------------------------


! - types -----------------------------------------------------------------------------------------
class(Input_Data_tp) :: this
!class(AnalysisData_tp) :: self
type(AnalysisData_tp) :: AnalysisInfo


! Local variables =================================================================================
! - integer variables -----------------------------------------------------------------------------
integer(kind=Smll) :: UnFile         ! Holds Unit of a file for error message
integer(kind=Smll) :: IO_File        ! For IOSTAT: Input Output Status in OPEN command
integer(kind=Smll) :: IO_write       ! Used for IOSTAT: Input/Output Status in the write command

integer(kind=Smll) :: i_analyses     ! loop index to read the analyses files

! - character variables ---------------------------------------------------------------------------
character (kind = 1, Len = 30) :: extfile

! code ============================================================================================
write(*,       *) " subroutine < Python_Visualizer_sub >: "
write(FileInfo,*) " subroutine < Python_Visualizer_sub >: "

! - Opening the domain file -----------------------------------------------------------------------


write(*,       *) " -Writing down the address file for Python visualizer script(Address.VisPy)... "
write(FileInfo,*) " -Writing down the address file for Python visualizer script(Address.VisPy)... "

UnFile = FilePythonAddress
open(unit=UnFile, file=trim(this%ModelName)//'_'// &
                       trim(this%AnalysesNames(i_analyses))//'_s'// &
                       trim(adjustL(this%IndexSize))//'.VisPy', &
err=1001, iostat=IO_File, access='sequential', action='write', asynchronous='no', blank='NULL', &
blocksize=0, defaultfile=trim(this%VisualizerDir), dispose='keep', form='formatted',&
position='asis', status='replace')

UnFile = FilePythonAddress
write(unit=UnFile,fmt="(' This is the address file for python visulaizer script: ')", &
      advance='yes',asynchronous='no', iostat=IO_write, err=1006)
write(unit=UnFile,fmt="(' ')", advance='yes',asynchronous='no', iostat=IO_write, err=1006)


write(unit=UnFile, fmt="(' file name is: ')", advance='yes', asynchronous='no', &
      iostat=IO_write, err=1006)
write(unit=UnFile,fmt="(A)",advance='yes',asynchronous='no', &
      iostat=IO_write,err=1006) this%ModelName
write(unit=UnFile,fmt="(' ')", advance='yes', asynchronous='no', iostat=IO_write, err=1006)


write(unit=UnFile, fmt="(' Analysis file name is: ')", advance='yes', asynchronous='no', &
      iostat=IO_write, err=1006)
write(unit=UnFile,fmt="(A)",advance='yes',asynchronous='no', &
      iostat=IO_write,err=1006) this%AnalysesNames(i_analyses)
write(unit=UnFile,fmt="(' ')", advance='yes', asynchronous='no', iostat=IO_write, err=1006)


write(unit=UnFile, fmt="(' No. of ranks: ')", advance='yes', asynchronous='no', &
      iostat=IO_write, err=1006)
write(unit=UnFile,fmt="(I20)",advance='yes',asynchronous='no', &
      iostat=IO_write,err=1006) this%size
write(unit=UnFile,fmt="(' ')", advance='yes', asynchronous='no', iostat=IO_write, err=1006)



write(unit=UnFile, fmt="(' Time step: ')", advance='yes', asynchronous='no', &
      iostat=IO_write, err=1006)
write(unit=UnFile,fmt="(F30.15)",advance='yes',asynchronous='no', &
      iostat=IO_write,err=1006) AnalysisInfo%TimeStep
write(unit=UnFile,fmt="(' ')", advance='yes', asynchronous='no', iostat=IO_write, err=1006)




write(unit=UnFile, fmt="(' Number of steps: ')", advance='yes', asynchronous='no', &
      iostat=IO_write, err=1006)
write(unit=UnFile,fmt="(I20)",advance='yes',asynchronous='no', &
      iostat=IO_write,err=1006) int(AnalysisInfo%TotalTime / AnalysisInfo%TimeStep, kind=Lng)
write(unit=UnFile,fmt="(' ')", advance='yes', asynchronous='no', iostat=IO_write, err=1006)




write(unit=UnFile, fmt="(' Plot increment is: ')", advance='yes', asynchronous='no', &
      iostat=IO_write, err=1006)
write(unit=UnFile,fmt="(I20)",advance='yes',asynchronous='no', &
      iostat=IO_write,err=1006) AnalysisInfo%Plot_Inc
write(unit=UnFile,fmt="(' ')", advance='yes', asynchronous='no', iostat=IO_write, err=1006)


! - Closing the domain file -----------------------------------------------------------------------
UnFile = FilePythonAddress
close(unit=UnFile, status="keep", err=1002, iostat=IO_File)

write(*,       *) " end subroutine < Python_Visualizer_sub >"
write(FileInfo,*) " end subroutine < Python_Visualizer_sub >"
return

! Errors ==========================================================================================
! Opening statement Errors
1001 call errorMessage(UnFile, IO_File)

! Close statement Errors
1002 call error_in_closing_a_file(UnFile, IO_File)

! write statement errors
1006 call error_in_writing(UnFile, IO_write)

end subroutine Python_Visualizer_sub

end module Input_mod
