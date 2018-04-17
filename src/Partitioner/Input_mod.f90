
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
! V1.00: 04/10/2018 - Major modifications
! V2.00: 04/17/2018 - Partitioner
!
! File version $Id $
!
! Last update: 04/17/2018
!
! ================================ S U B R O U T I N E ============================================
! Input_Address_sub: Reads file name and directories from the address file.
! Input_Basic_sub
! Input_Array_sub
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

  real(kind=SGL) :: Version               ! Holds the version of the code.

  contains
    procedure Input => Input_Address_sub

end type Input_Data_tp


! Contains all information about the geometry of the domain. (input)
type Geometry_tp
  integer(kind=Lng) :: NoReaches ! Number of reaches
  integer(kind=Shrt):: size ! Number of reaches

  integer(kind=Lng),  allocatable, dimension(:) :: ReachDisc ! no. of control volume in each reach
  integer(kind=Shrt), allocatable, dimension(:) :: ReachType ! reach type

  real(kind=DBL), allocatable, dimension(:) :: ReachLength  ! the length of each reach
  real(kind=DBL), allocatable, dimension(:) :: ReachSlope   ! the slope of each reach
  real(kind=DBL), allocatable, dimension(:) :: ReachManning ! the Manning's number for each reach
  real(kind=DBL), allocatable, dimension(:) :: ReachWidth   ! Stores the width of each reach

  contains
    procedure Basic => Input_Basic_sub
    procedure Array => Input_Array_sub

end type Geometry_tp


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
! Purpose: This subroutine reads the initial information for simulation, so that the arrays can be
!          allocated.
!
! Developed by: Babak Poursartip
! Supervised by: Clint Dawson
!
! The Institute for Computational Engineering and Sciences (ICES)
! The University of Texas at Austin
!
! ================================ V E R S I O N ==================================================
! V0.10: 02/21/2018 - Initiation.
! V1.00: 03/01/2018 - Compiled without error.
! V1.10: 04/10/2018 - Minor modifications.
!
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

Subroutine Input_Basic_sub(this, ModelInfo)

! Libraries =======================================================================================

! User defined modules ============================================================================

Implicit None

! Global Variables ================================================================================

! - Types -----------------------------------------------------------------------------------------
type(Input_Data_tp),  intent(In)  :: ModelInfo ! Holds info. (name, dir, output dir) of the model
class(Geometry_tp),   intent(out) :: this  ! Holds information about the geometry of the domain

! Local Variables =================================================================================
! - integer Variables -----------------------------------------------------------------------------
integer(kind=Smll) :: UnFile   ! Holds Unit of a file for error message
integer(kind=Smll) :: IO_File  ! For IOSTAT: Input Output status in OPEN command
integer(kind=Smll) :: IO_read  ! Holds error of read statements
integer(kind=Smll) :: IO_write ! Used for IOSTAT - Input Output Status - in the write command.

! code ============================================================================================
write(*,       *)
write(*,       *) " Subroutine < Input_Basic_sub >: "
write(FileInfo,*)
write(FileInfo,*) " Subroutine < Input_Basic_sub >: "

! - Opening the data model file -------------------------------------------------------------------
write(*,        fmt="(A)") " -Opening the data model file ..."
write(FileInfo, fmt="(A)") " -Opening the data model file ..."

print*, ModelInfo%ModelName
print*, ModelInfo%InputDir

UnFile=FileDataModel
open(Unit=UnFile, file=trim(ModelInfo%ModelName)//'.dataModel', &
     err=1001, iostat=IO_File, access='sequential', action='read', asynchronous='no', &
     blank='null', blocksize=0, defaultfile=trim(ModelInfo%InputDir), DisPOSE='keep', form='formatted', &
     position='asis', status='old')

UnFile = FileDataModel  ! Total number of reaches in the domain
read(unit=UnFile, fmt="(A)", advance='yes', asynchronous='no', iostat=IO_read, err=1003, end=1004)
read(unit=UnFile, fmt="(A)", advance='yes', asynchronous='no', iostat=IO_read, err=1003, end=1004)
read(unit=UnFile, fmt="(A)", advance='yes', asynchronous='no', iostat=IO_read, err=1003, end=1004)
read(unit=UnFile, fmt="(I10)", advance='yes', asynchronous='no', iostat=IO_read, err=1003, end=1004) this%NoReaches
UnFile = FileInfo
write(unit=UnFile, fmt="(' Total number of reach(es) is(are): ', I10)", advance='yes', asynchronous='no', iostat=IO_write, err=1006) this%NoReaches
write(unit=*,      fmt="(' Total number of reach(es) is(are): ', I10)") this%NoReaches

UnFile = FileDataModel  ! Number of cores - required for mesh partitioning.
read(unit=UnFile, fmt="(A)", advance='yes', asynchronous='no', iostat=IO_read, err=1003, end=1004)
read(unit=UnFile, fmt="(A)", advance='yes', asynchronous='no', iostat=IO_read, err=1003, end=1004)
read(unit=UnFile, fmt="(I10)", advance='yes', asynchronous='no', iostat=IO_read, err=1003, end=1004) this%size
UnFile = FileInfo
write(unit=UnFile, fmt="(' Total number of core(s) is(are): ', I10)", advance='yes', asynchronous='no', iostat=IO_write, err=1006) this%size
write(unit=*,      fmt="(' Total number of core(s) is(are): ', I10)") this%NoReaches

! - Closing the data model file -------------------------------------------------------------------
write(*,        fmt="(A)") " -Closing the data model file"
write(FileInfo, fmt="(A)") " -Closing the data model file"

UnFile =  FileDataModel
Close(Unit = UnFile, status = 'KEEP', ERR =  1002, IOSTAT = IO_File)

write(*,       *) ' End Subroutine < Input_Basic_sub >'
write(*,       *)
write(FileInfo,*) ' End Subroutine < Input_Basic_sub >'
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


End Subroutine Input_Basic_sub


!##################################################################################################
! Purpose: This subroutine reads the arrays.
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

Subroutine Input_Array_sub(this, ModelInfo)

! Libraries =======================================================================================

! User defined modules ============================================================================

Implicit None

! Global Variables ================================================================================

! - types -----------------------------------------------------------------------------------------
type(Input_Data_tp), intent(In)    :: ModelInfo  ! Holds info. (name, dir, output dir) of the model
class(Geometry_tp),  intent(inout) :: this     ! Holds information about the geometry of the domain

! Local Variables =================================================================================
! - integer Variables -----------------------------------------------------------------------------
integer(kind=Smll) :: UnFile   ! Holds Unit of a file for error message
integer(kind=Smll) :: IO_File  ! For IOSTAT: Input Output status in OPEN command
integer(kind=Smll) :: IO_read  ! Holds error of read statements
integer(kind=Smll) :: IO_write ! Used for IOSTAT - Input Output Status - in the write command
integer(kind=Smll) :: i_reach  ! loop index on the number of reaches

! code ============================================================================================
write(*,       *)
write(*,       *) " Subroutine < Input_Array_sub >: "
write(FileInfo,*)
write(FileInfo,*) " Subroutine < Input_Array_sub >: "

! Open required Files -----------------------------------------------------------------------------
write(*,        fmt="(A)") " -Opening the input files for arrays ..."
write(FileInfo, fmt="(A)") " -Opening the input files for arrays ..."

! Open the input file for arrays
UnFile = FileDataGeo
open(Unit=UnFile, file=trim(ModelInfo%ModelName)//'.Geo', &
     err=1001, iostat=IO_File, access='sequential', action='read', asynchronous='no', &
     blank='null', blocksize=0, defaultfile=trim(ModelInfo%InputDir), DisPOSE='keep', form='formatted', &
     position='asis', status='old')

UnFile = FileDataGeo
read(unit=UnFile, fmt="(A)", advance='yes', asynchronous='no', iostat=IO_read, err=1003, end=1004)
read(unit=UnFile, fmt="(A)", advance='yes', asynchronous='no', iostat=IO_read, err=1003, end=1004)

! Reading the length of each reach
read(unit=UnFile, fmt="(A)", advance='yes', asynchronous='no', iostat=IO_read, err=1003, end=1004)
  do i_reach= 1, this%NoReaches
    UnFile = FileDataGeo
    read(unit=UnFile, fmt="(F23.10)", advance='yes', asynchronous='no', iostat=IO_read, err=1003, end=1004) this%ReachLength(i_reach); !write(*,*)this%ReachLength(i_reach)
    UnFile = FileInfo
    write(unit=*,      fmt="(' The length of reach ', I5, ' is:', F23.10,' m')") i_reach, this%ReachLength(i_reach)
    write(unit=UnFile, fmt="(' The length of reach ', I5, ' is:', F23.10,' m')") i_reach, this%ReachLength(i_reach)
  end do

! Reading total number of control volumes in each reach/ For now we have a constant discretization in each reach.
UnFile = FileDataGeo
read(unit=UnFile, fmt="(A)", advance='yes', asynchronous='no', iostat=IO_read, err=1003, end=1004)
read(unit=UnFile, fmt="(A)", advance='yes', asynchronous='no', iostat=IO_read, err=1003, end=1004)
  do i_reach= 1, this%NoReaches
    UnFile = FileDataGeo
    read(unit=UnFile, fmt="(I10)", advance='yes', asynchronous='no', iostat=IO_read, err=1003, end=1004) this%ReachDisc(i_reach)
    UnFile = FileInfo
    write(unit=*,      fmt="(' No. of discretization of reach ', I5, ' is:', I10)") i_reach, this%ReachDisc(i_reach)
    write(unit=UnFile, fmt="(' No. of discretization of reach ', I5, ' is:', I10)") i_reach, this%ReachDisc(i_reach)
  end do

! Reading reach type
UnFile = FileDataGeo
read(unit=UnFile, fmt="(A)", advance='yes', asynchronous='no', iostat=IO_read, err=1003, end=1004)
read(unit=UnFile, fmt="(A)", advance='yes', asynchronous='no', iostat=IO_read, err=1003, end=1004)
  do i_reach= 1, this%NoReaches
    UnFile = FileDataGeo
    read(unit=UnFile, fmt="(I10)", advance='yes', asynchronous='no', iostat=IO_read, err=1003, end=1004) this%ReachType(i_reach)
    UnFile = FileInfo
    write(unit=*,      fmt="(' Rreach ', I10,' is of type: ', I5)") i_reach, this%ReachType(i_reach)
    write(unit=UnFile, fmt="(' Rreach ', I10,' is of type: ', I5)") i_reach, this%ReachType(i_reach)
  end do

! Reading slopes of reach
UnFile = FileDataGeo
read(unit=UnFile, fmt="(A)", advance='yes', asynchronous='no', iostat=IO_read, err=1003, end=1004)
read(unit=UnFile, fmt="(A)", advance='yes', asynchronous='no', iostat=IO_read, err=1003, end=1004)
  do i_reach= 1, this%NoReaches
    UnFile = FileDataGeo
    read(unit=UnFile, fmt="(F23.10)", advance='yes', asynchronous='no', iostat=IO_read, err=1003, end=1004) this%ReachSlope(i_reach)
    UnFile = FileInfo
    write(unit=*,      fmt="(' The slope of reach ', I10,' is: ', F23.10)") i_reach, this%ReachSlope(i_reach)
    write(unit=UnFile, fmt="(' The slope of reach ', I10,' is: ', F23.10)") i_reach, this%ReachSlope(i_reach)
  end do

! Reading Manning number of each reach
UnFile = FileDataGeo
read(unit=UnFile, fmt="(A)", advance='yes', asynchronous='no', iostat=IO_read, err=1003, end=1004)
read(unit=UnFile, fmt="(A)", advance='yes', asynchronous='no', iostat=IO_read, err=1003, end=1004)
  do i_reach= 1, this%NoReaches
    UnFile = FileDataGeo
    read(unit=UnFile, fmt="(F23.10)", advance='yes', asynchronous='no', iostat=IO_read, err=1003, end=1004) this%ReachManning(i_reach)
    UnFile = FileInfo
    write(unit=*,      fmt="(' The Mannings no. for reach ', I10,' is: ', F23.10)") i_reach, this%ReachManning(i_reach)
    write(unit=UnFile, fmt="(' The Mannings no. for reach ', I10,' is: ', F23.10)") i_reach, this%ReachManning(i_reach)
  end do

! Reading the width of each reach
UnFile = FileDataGeo
read(unit=UnFile, fmt="(A)", advance='yes', asynchronous='no', iostat=IO_read, err=1003, end=1004)
read(unit=UnFile, fmt="(A)", advance='yes', asynchronous='no', iostat=IO_read, err=1003, end=1004)
  do i_reach= 1, this%NoReaches
    UnFile = FileDataGeo
    read(unit=UnFile, fmt="(F23.10)", advance='yes', asynchronous='no', iostat=IO_read, err=1003, end=1004) this%ReachWidth(i_reach)
    UnFile = FileInfo
    write(unit=*,      fmt="(' The width of reach ', I10,'is: ', F23.10)") i_reach, this%ReachWidth(i_reach)
    write(unit=UnFile, fmt="(' The width of reach ', I10,'is: ', F23.10)") i_reach, this%ReachWidth(i_reach)
  end do

! - Closing the geometry file ---------------------------------------------------------------------
write(*,        fmt="(A)") " -Closing the geometry file"
write(FileInfo, fmt="(A)") " -Closing the geometry file"

UnFile = FileDataGeo
Close(Unit = UnFile, status = 'KEEP', ERR =  1002, IOSTAT = IO_File)

write(*,       *) " End Subroutine < Input_Array_sub >"
write(*,       *)
write(FileInfo,*) " End Subroutine < Input_Array_sub >"
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

end Subroutine Input_Array_sub

end module Input_mod
