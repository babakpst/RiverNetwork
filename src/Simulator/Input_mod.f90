
!##################################################################################################
! Purpose: Reads file name and directories from the address file.
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
!
! File version $Id $
!
! Last update: 05/15/2018
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

implicit none


! Holds info. (name, dir. output dir.) of the model, initialized by subroutine Input_Address_sub.
type Input_Data_tp
  character (kind = 1, Len = 30 ) :: ModelName     ! Name of the model input file
  character (kind = 1, Len = 150) :: InputDir      ! Directory of the input file.
  character (kind = 1, Len = 150) :: AnalysisDir   ! Directory of Analysis input file.
  character (kind = 1, Len = 150) :: OutputDir     ! Directory of output files (Results)
  character (kind = 1, Len = 150) :: AnalysisOutputDir! Directory of output file for each analysis
  character (kind = 1, Len = 150), dimension(:), allocatable :: AnalysesNames! Holds the names of
                                                                        ! the analysis input files
  !integer(kind=Smll):: OutputType        ! Output Type: 1: ordinary-2: HDF5
  integer(kind=Smll) :: NumberOfAnalyses  ! Number of analysis
  integer(kind=Smll) :: size              ! Number of partitions

  real(kind=SGL) :: Version               ! Holds the version of the code.

  contains
    procedure :: Input => Input_Address_sub
end type Input_Data_tp

! Contains all information about the domain, required
type AnalysisData_tp
  integer(kind=Smll) :: AnalysisType ! Analysis Type      -1:1D Lax-Wendroff
                                     !                    -2:1D Lax-Wendroff with limiter
  integer(kind=Smll) :: limiter      ! limiter type

  real(kind=DBL):: TotalTime ! Total simulation time (in seconds)
  real(kind=DBL):: TimeStep  ! Time Step
  real(kind=DBL):: Q_Up      ! Upstream boundary condition, constant flow (m^3/s)
  real(kind=DBL):: h_dw      ! Downstream water depth (in meters)
  real(kind=DBL):: CntrlV    ! Initial control volume
  real(kind=DBL):: CntrlV_ratio  ! Initial control volume ration, used to initialize data

  contains
    procedure:: Analysis => Input_Analysis_sub

end type AnalysisData_tp


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
read(FileAdr,*) this%ModelName; write(*,*) this%ModelName
read(FileAdr,*)
read(FileAdr,*)
read(FileAdr,*) this%InputDir; !write(*,*) this%InputDir
read(FileAdr,*)
read(FileAdr,*)
read(FileAdr,*) this%OutputDir; !write(*,*) this%OutputDir
read(FileAdr,*)
read(FileAdr,*)
read(FileAdr,*) this%size;      !write(*,*) this%size
read(FileAdr,*)
read(FileAdr,*)
read(FileAdr,*) this%NumberOfAnalyses; !write(*,*) this%NumberOfAnalyses

! Allocating
allocate(this%AnalysesNames(this%NumberOfAnalyses),  stat=ERR_Alloc)
  if (ERR_Alloc /= 0) then
    write(*, Fmt_ALLCT) ERR_Alloc ;  write(FileInfo, Fmt_ALLCT) ERR_Alloc;
    write(*, Fmt_FL);  write(FileInfo, Fmt_FL); read(*, Fmt_End);  stop;
  end if
read(FileAdr,*)
read(FileAdr,*)
  do i_analyses = 1, this%NumberOfAnalyses
    read(FileAdr,*) this%AnalysesNames(i_analyses)
  end do

this%AnalysisDir=trim(AdjustL(this%InputDir))//'/'// &
                      trim(AdjustL(this%ModelName))//'/'//'Analysis'
this%InputDir   =trim(AdjustL(this%InputDir))//'/'// &
                      trim(AdjustL(this%ModelName))//'/'//'Model'

write(*, fmt="(2A)")" The model directory is: ", this%InputDir
write(*, fmt="(2A)")" The analysis name is: ", this%AnalysisDir

! Create the results folder
write(*,fmt="(A)") " -Creating the output folders ..."

Directory=MakeDirQQ (trim(AdjustL(this%OutputDir))//'/'//trim(AdjustL(this%ModelName)))
  if (Directory) then
    write(*,fmt="(A)") " The result folder is created."
  Else
     write(*,fmt="(A)") " The result folder already exists."
  end if

this%OutputDir=trim(AdjustL (this%OutputDir))//'/'//trim(AdjustL (this%ModelName))

write(*,fmt="(2A)")" The output directory is: ", this%OutputDir

! - Closing the address file ----------------------------------------------------------------------
write(*,        fmt="(A)") " -Closing the address file"
!write(FileInfo, fmt="(A)") " -Closing the address file"
UnFile =  FileAdr
Close(unit = UnFile, status = 'KEEP', ERR =  1002, IOSTAT = IO_File)


write(*,       *) ' End Subroutine < Input_Address_sub >'
write(*,       *)
!write(FileInfo,*) ' End Subroutine < Input_Address_sub >'
!write(FileInfo,*)
Return

! Errors ==========================================================================================
! Opening statement Errors
1001 if (IO_File > 0) then
       write(*, Fmt_Err1_OPEN) UnFile, IO_File; write(FileInfo, Fmt_Err1_OPEN) UnFile, IO_File;
       write(*, Fmt_FL); write(FileInfo, Fmt_FL);
       write(*, Fmt_End); read(*,*); stop;
     Else if ( IO_File < 0 ) then
       write(*, Fmt_Err1_OPEN) UnFile, IO_File
       write(FileInfo, Fmt_Err1_OPEN) UnFile, IO_File;  write(*, Fmt_FL); write(FileInfo, Fmt_FL);
       write(*, Fmt_End); read(*,*); stop;
     end if

! Close statement Errors
1002 if (IO_File > 0) then
       write(*, Fmt_Err1_Close) UnFile, IO_File; write(FileInfo, Fmt_Err1_Close) UnFile, IO_File;
       write(*, Fmt_FL); write(FileInfo, Fmt_FL);
       write(*, Fmt_End); read(*,*); stop;
     end if

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

print*, ModelInfo%AnalysesNames(i_analyses)
print*, ModelInfo%AnalysisDir

UnFile=UnInptAna
Open(Unit=UnFile, File=trim(ModelInfo%AnalysesNames(i_analyses))//'.Analysis', Err= 1001, IOStat=IO_File, Access='sequential', &
      action='read', Asynchronous='no', blank='null', blocksize=0, defaultfile=trim(ModelInfo%AnalysisDir), &
      dispose='Keep', form='formatted', position='asis', status='old') ;

! Creating the output file directory for this analysis --------------------------------------------
write(*,        fmt="(A)") " -Creating the output folder for this analysis ..."
write(FileInfo, fmt="(A)") " -Creating the output folder for this analysis ..."

Directory=MakeDirQQ (trim(AdjustL (ModelInfo%OutputDir))//'/'//trim(AdjustL(ModelInfo%AnalysesNames(i_analyses))))
  if (Directory) then ;
     write(*,       fmt="(A)") "The output folder for this analysis created." ;
     write(FileInfo,fmt="(A)") "The output folder for this analysis created." ;
  Else ;
     write(*,        fmt="(A)") "The output folder for this analysis already exists." ;
     write(FileInfo, fmt="(A)") "The output folder for this analysis already exists." ;
  end if ;

ModelInfo%AnalysisOutputDir=trim(AdjustL(ModelInfo%OutputDir))//'/'//trim(AdjustL(ModelInfo%AnalysesNames(i_analyses)))

print*,"check 000", ModelInfo%AnalysisOutputDir
UnFile = UnInptAna
read(unit=UnFile, fmt="(A)", advance='yes', asynchronous='no', iostat=IO_read, err=1003, end=1004)
read(unit=UnFile, fmt="(A)", advance='yes', asynchronous='no', iostat=IO_read, err=1003, end=1004)
read(unit=UnFile, fmt="(A)", advance='yes', asynchronous='no', iostat=IO_read, err=1003, end=1004)
read(unit=UnFile, fmt="(I10)", advance='yes', asynchronous='no', iostat=IO_read, err=1003, end=1004) this%AnalysisType
UnFile = FileInfo
write(unit=*,      fmt="(' The Analysis type is: ', I10)") this%AnalysisType
write(unit=UnFile, fmt="(' The Analysis type is: ', I10)", advance='yes', asynchronous='no', iostat=IO_write, err=1006) this%AnalysisType

UnFile = UnInptAna
read(unit=UnFile, fmt="(A)", advance='yes', asynchronous='no', iostat=IO_read, err=1003, end=1004)
read(unit=UnFile, fmt="(A)", advance='yes', asynchronous='no', iostat=IO_read, err=1003, end=1004)
read(unit=UnFile, fmt="(F23.10)", advance='yes', asynchronous='no', iostat=IO_read, err=1003, end=1004) this%TotalTime
UnFile = FileInfo
write(unit=*,      fmt="(' The total simulation time is: ', F23.10, ' s')") this%TotalTime
write(unit=UnFile, fmt="(' The total simulation time is: ', F23.10, ' s')", advance='yes', asynchronous='no', iostat=IO_write, err=1006) this%TotalTime

UnFile = UnInptAna
read(unit=UnFile, fmt="(A)", advance='yes', asynchronous='no', iostat=IO_read, err=1003, end=1004)
read(unit=UnFile, fmt="(A)", advance='yes', asynchronous='no', iostat=IO_read, err=1003, end=1004)
read(unit=UnFile, fmt="(F23.10)", advance='yes', asynchronous='no', iostat=IO_read, err=1003, end=1004) this%TimeStep
UnFile = FileInfo
write(unit=UnFile, fmt="(' The time step is: ', F23.10, ' s')", advance='yes', asynchronous='no', iostat=IO_write, err=1006) this%TimeStep
write(unit=*,      fmt="(' The time step is: ', F23.10, ' s')") this%TimeStep

UnFile = UnInptAna
read(unit=UnFile, fmt="(A)", advance='yes', asynchronous='no', iostat=IO_read, err=1003, end=1004)
read(unit=UnFile, fmt="(A)", advance='yes', asynchronous='no', iostat=IO_read, err=1003, end=1004)
read(unit=UnFile, fmt="(F23.10)", advance='yes', asynchronous='no', iostat=IO_read, err=1003, end=1004) this%Q_Up
UnFile = FileInfo
write(unit=UnFile, fmt="(' Flow rate at the upstream is: ', F23.10, ' m/s3')", advance='yes', asynchronous='no', iostat=IO_write, err=1006) this%Q_Up
write(unit=*,      fmt="(' Flow rate at the upstream is: ', F23.10, ' m/s3')") this%Q_Up

UnFile = UnInptAna
read(unit=UnFile, fmt="(A)", advance='yes', asynchronous='no', iostat=IO_read, err=1003, end=1004)
read(unit=UnFile, fmt="(A)", advance='yes', asynchronous='no', iostat=IO_read, err=1003, end=1004)
read(unit=UnFile, fmt="(F23.10)", advance='yes', asynchronous='no', iostat=IO_read, err=1003, end=1004) this%h_dw
UnFile = FileInfo
write(unit=UnFile, fmt="(' Downstream water depth is: ', F23.10, ' m')", advance='yes', asynchronous='no', iostat=IO_write, err=1006) this%h_dw
write(unit=*,      fmt="(' Downstream water depth is: ', F23.10, ' m')") this%h_dw

UnFile = UnInptAna
read(unit=UnFile, fmt="(A)", advance='yes', asynchronous='no', iostat=IO_read, err=1003, end=1004)
read(unit=UnFile, fmt="(A)", advance='yes', asynchronous='no', iostat=IO_read, err=1003, end=1004)
read(unit=UnFile, fmt="(F23.10)", advance='yes', asynchronous='no', iostat=IO_read, err=1003, end=1004) this%CntrlV
UnFile = FileInfo
write(unit=UnFile, fmt="(' Control Volume is: ', F23.10, ' m^3')", advance='yes', asynchronous='no', iostat=IO_write, err=1006) this%CntrlV
write(unit=*,      fmt="(' Control Volume is: ', F23.10, ' m^3')") this%CntrlV

UnFile = UnInptAna
read(unit=UnFile, fmt="(A)", advance='yes', asynchronous='no', iostat=IO_read, err=1003, end=1004)
read(unit=UnFile, fmt="(A)", advance='yes', asynchronous='no', iostat=IO_read, err=1003, end=1004)
read(unit=UnFile, fmt="(F23.10)", advance='yes', asynchronous='no', iostat=IO_read, err=1003, end=1004) this%CntrlV_ratio
UnFile = FileInfo
write(unit=UnFile, fmt="(' The ratio of control volume is: ', F23.10)", advance='yes', asynchronous='no', iostat=IO_write, err=1006) this%CntrlV_ratio
write(unit=*,      fmt="(' The ratio of control volume is: ', F23.10)") this%CntrlV_ratio

UnFile = UnInptAna
read(unit=UnFile, fmt="(A)", advance='yes', asynchronous='no', iostat=IO_read, err=1003, end=1004)
read(unit=UnFile, fmt="(A)", advance='yes', asynchronous='no', iostat=IO_read, err=1003, end=1004)
read(unit=UnFile, fmt="(I10)", advance='yes', asynchronous='no', iostat=IO_read, err=1003, end=1004) this%limiter
UnFile = FileInfo
write(unit=UnFile, fmt="(' The limiter is: ', I10)", advance='yes', asynchronous='no', iostat=IO_write, err=1006) this%limiter
write(unit=*,      fmt="(' The limiter is: ', I10)") this%limiter


write(*,       *) " End Subroutine < Input_Analysis_sub >"
write(*,       *)
write(FileInfo,*) " End Subroutine < Input_Analysis_sub >"
write(FileInfo,*)

Return

! Errors ==========================================================================================
! Opening statement Errors
1001 if (IO_File > 0) then
       write(*, Fmt_Err1_OPEN) UnFile, IO_File; write(FileInfo, Fmt_Err1_OPEN) UnFile, IO_File;
       write(*, Fmt_FL); write(FileInfo, Fmt_FL);
       write(*, Fmt_End); read(*,*); stop;
     Else if ( IO_File < 0 ) then
       write(*, Fmt_Err1_OPEN) UnFile, IO_File
       write(FileInfo, Fmt_Err1_OPEN) UnFile, IO_File;  write(*, Fmt_FL) ; write(FileInfo, Fmt_FL);
       write(*, Fmt_End); read(*,*); stop;
     end if


! Close statement Errors
1002 if (IO_File > 0) then
       write(*, Fmt_Err1_Close) UnFile, IO_File; write(FileInfo, Fmt_Err1_Close) UnFile, IO_File;
       write(*, Fmt_FL); write(FileInfo, Fmt_FL);
       write(*, Fmt_End); read(*,*); stop;
     end if

! - Error in read statement -----------------------------------------------------------------------
1003 write(*, Fmt_read1) UnFile, IO_read; write(UnFile, Fmt_read1) UnFile, IO_read;
     write(*, Fmt_FL);  write(FileInfo, Fmt_FL); write(*, Fmt_End); read(*,*);  stop;

! - End-OF-FILE in read statement -----------------------------------------------------------------
1004 write(*, Fmt_read2) UnFile, IO_read; write(UnFile, Fmt_read2) UnFile, IO_read;
     write(*, Fmt_FL);  write(FileInfo, Fmt_FL); write(*, Fmt_End); read(*,*);  stop;

! - End-OF-FILE IN read statement -----------------------------------------------------------------
1005 write(*, Fmt_read3) UnFile, IO_read; write(UnFile, Fmt_read3) UnFile, IO_read;
     write(*, Fmt_FL);  write(FileInfo, Fmt_FL); write(*, Fmt_End); read(*,*);  stop;

! - write statement error -------------------------------------------------------------------------
1006 write(*, Fmt_write1) UnFile, IO_write; write(UnFile, Fmt_write1) UnFile, IO_write;
     write(*, Fmt_FL); write(FileInfo, Fmt_FL); write(*, Fmt_End); read(*,*);  stop;

End Subroutine Input_Analysis_sub


end module Input_mod
