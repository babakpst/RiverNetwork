
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
! V0.1: 02/22/2018 - Initiation.
! V0.10: 03/08/2018 - Initiated: Compiled without error.
!
! File version $Id $
!
! Last update: 02/23/2018
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


interface Input
  module procedure Input_Address_sub, Input_Analysis_sub!, Input_Basic_sub, Input_Array_sub
end interface Input

contains


!##################################################################################################
! Purpose: This subroutine/function ....
!
! Developed by: Babak Poursartip
! Supervised by:
!
! The Institute for Computational Engineering and Sciences (ICES)
! The University of Texas at Austin
!
! ================================ V E R S I O N ==================================================
! V0.1: 02/21/2018 - Initiation.
!
! File version $Id $
!
! Last update:
!
! ================================ L O C A L   V A R I A B L E S ==================================
! (Refer to the main code to see the list of imported variables)
!  . . . . . . . . . . . . . . . . Variables . . . . . . . . . . . . . . . . . . . . . . . . . . .
!
!##################################################################################################

Subroutine Input_Address_sub(                                         &
!                                                                     & ! integer(1) Variables
!                                                                     & ! integer(2) Variables
!                                                                     & ! integer(4) Variables
!                                                                     & ! integer(8) Variables
!                                                                     & ! real Variables
!                                                                     & ! integer Arrays
!                                                                     & ! real Arrays
!                                                                     & ! Characters
ModelInfo                                                             & ! Type
)


! Libraries =======================================================================================
use ifport

! User defined modules ============================================================================


Implicit None

! Global Variables ================================================================================

! - integer Variables -----------------------------------------------------------------------------
!#integer(kind=Shrt), intent(In)    ::
!#integer(kind=Shrt), intent(InOut) ::
!#integer(kind=Shrt), intent(OUT)   ::
! - real Variables --------------------------------------------------------------------------------
!#real(kind=Dbl),     intent(In)    ::
!#real(kind=Dbl),     intent(InOut) ::
!#real(kind=Dbl),     intent(OUT)   ::
! - complex Variables -----------------------------------------------------------------------------
!#complex,             intent(In)    ::
!#complex,             intent(InOut) ::
!#complex,             intent(OUT)   ::
! - integer Arrays --------------------------------------------------------------------------------
!#integer(kind=Shrt), intent(In),    dimension(:  )  ::
!#integer(kind=Shrt), intent(In),    dimension(:,:)  ::
!#integer(kind=Shrt), intent(In)    ::
!#integer(kind=Shrt), intent(InOut) ::
!#integer(kind=Shrt), intent(OUT)   ::
! - real Arrays -----------------------------------------------------------------------------------
!#real(kind=Dbl),     intent(In),    dimension(:  )  ::
!#real(kind=Dbl),     intent(InOut), dimension(:  )  ::
!#real(kind=Dbl),     intent(OUT),   dimension(:  )  ::
! - Character Variables ---------------------------------------------------------------------------
!#Character (kind = ?, Len = ? ) ::
! - Logical Variables -----------------------------------------------------------------------------
!#Logical   ::
! - Types -----------------------------------------------------------------------------------------
type(Input_Data_tp), intent(out) :: ModelInfo  ! Holds info. (name, dir, output dir) of the model

! Local Variables =================================================================================
! - integer Variables -----------------------------------------------------------------------------
integer(kind=Smll) :: UnFile        ! Holds Unit of a file for error message
integer(kind=Smll) :: IO_File       ! For IOSTAT: Input Output status in OPEN command
integer(kind=Smll) :: i_analyses    ! loop index to read the analyses files
integer(kind=Smll) :: ERR_Alloc, ERR_DeAlloc ! Allocating and DeAllocating errors

! - real Variables --------------------------------------------------------------------------------
!#real(kind=Dbl)      ::
! - complex Variables -----------------------------------------------------------------------------
!#complex              ::
! - integer Arrays --------------------------------------------------------------------------------
!#integer(kind=Shrt), dimension(:)  ::
!#integer(kind=Shrt), allocatable, dimension(:)  ::
! - real Arrays -----------------------------------------------------------------------------------
!#real(kind=Dbl), dimension(:)      ::
!#real(kind=Dbl), allocatable, dimension(:)  ::
! - Character Variables ---------------------------------------------------------------------------
!#Character (kind = ?, Len = ? ) ::
! - Logical Variables -----------------------------------------------------------------------------
Logical (kind=Shrt)  :: Directory

! - Type ------------------------------------------------------------------------------------------

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
read(FileAdr,*) ModelInfo%ModelName; write(*,*) ModelInfo%ModelName
read(FileAdr,*)
read(FileAdr,*)
read(FileAdr,*) ModelInfo%InputDir; !write(*,*) ModelInfo%InputDir
read(FileAdr,*)
read(FileAdr,*)
read(FileAdr,*) ModelInfo%OutputDir; !write(*,*) ModelInfo%OutputDir
read(FileAdr,*)
read(FileAdr,*)
read(FileAdr,*) ModelInfo%NumberOfAnalyses; !write(*,*) ModelInfo%NumberOfAnalyses

! Allocating
allocate(ModelInfo%AnalysesNames(ModelInfo%NumberOfAnalyses),  stat=ERR_Alloc)
  if (ERR_Alloc /= 0) then
    write(*, Fmt_ALLCT) ERR_Alloc ;  write(FileInfo, Fmt_ALLCT) ERR_Alloc;
    write(*, Fmt_FL);  write(FileInfo, Fmt_FL); read(*, Fmt_End);  stop;
  end if
read(FileAdr,*)
read(FileAdr,*)
  do i_analyses = 1, ModelInfo%NumberOfAnalyses
    read(FileAdr,*) ModelInfo%AnalysesNames(i_analyses)
  end do

ModelInfo%AnalysisDir=trim(AdjustL(ModelInfo%InputDir))//'/'// &
                      trim(AdjustL(ModelInfo%ModelName))//'/'//'Analysis'
ModelInfo%InputDir   =trim(AdjustL(ModelInfo%InputDir))//'/'// &
                      trim(AdjustL(ModelInfo%ModelName))//'/'//'Model'

write(*, fmt="(2A)")" The model directory is: ", ModelInfo%InputDir
write(*, fmt="(2A)")" The analysis name is: ", ModelInfo%AnalysisDir

! Create the results folder
write(*,fmt="(A)") " -Creating the output folders ..."

Directory=MakeDirQQ (trim(AdjustL(ModelInfo%OutputDir))//'/'//trim(AdjustL(ModelInfo%ModelName)))
  if (Directory) then
    write(*,fmt="(A)") " The result folder is created."
  Else
     write(*,fmt="(A)") " The result folder already exists."
  end if

ModelInfo%OutputDir=trim(AdjustL (ModelInfo%OutputDir))//'/'//trim(AdjustL (ModelInfo%ModelName))

write(*,fmt="(2A)")" The output directory is: ", ModelInfo%OutputDir

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
! V0.1: 02/21/2018 - Initiation.
!
! File version $Id $
!
! Last update: 02/21/2018
!
! ================================ L O C A L   V A R I A B L E S ==================================
! (Refer to the main code to see the list of imported variables)
!  . . . . . . . . . . . . . . . . Variables . . . . . . . . . . . . . . . . . . . . . . . . . . .
!
!##################################################################################################

Subroutine Input_Basic_sub(                                           &
!                                                                     & ! integer(1) Variables
!                                                                     & ! integer(2) Variables
!                                                                     & ! integer(4) Variables
!                                                                     & ! integer(8) Variables
!                                                                     & ! real Variables
!                                                                     & ! integer Arrays
!                                                                     & ! real Arrays
!                                                                     & ! Characters
ModelInfo, Geometry                                                   & ! Type
)

! Libraries =======================================================================================

! User defined modules ============================================================================

Implicit None

! Global Variables ================================================================================

! - integer Variables -----------------------------------------------------------------------------
!#integer(kind=Shrt), intent(In)    ::
!#integer(kind=Shrt), intent(InOut) ::
!#integer(kind=Shrt), intent(OUT)   ::
! - real Variables --------------------------------------------------------------------------------
!#real(kind=Dbl),     intent(In)    ::
!#real(kind=Dbl),     intent(InOut) ::
!#real(kind=Dbl),     intent(OUT)   ::
! - complex Variables -----------------------------------------------------------------------------
!#complex,             intent(In)    ::
!#complex,             intent(InOut) ::
!#complex,             intent(OUT)   ::
! - integer Arrays --------------------------------------------------------------------------------
!#integer(kind=Shrt), intent(In),    dimension(:  )  ::
!#integer(kind=Shrt), intent(In),    dimension(:,:)  ::
!#integer(kind=Shrt), intent(In)    ::
!#integer(kind=Shrt), intent(InOut) ::
!#integer(kind=Shrt), intent(OUT)   ::
! - real Arrays -----------------------------------------------------------------------------------
!#real(kind=Dbl),     intent(In),    dimension(:  )  ::
!#real(kind=Dbl),     intent(InOut), dimension(:  )  ::
!#real(kind=Dbl),     intent(OUT),   dimension(:  )  ::
! - Character Variables ---------------------------------------------------------------------------
!#Character (kind = ?, Len = ? ) ::
! - Logical Variables -----------------------------------------------------------------------------
!#Logical   ::
! - Types -----------------------------------------------------------------------------------------
type(Input_Data_tp),  intent(In)  :: ModelInfo   ! Holds info. (name, dir, output dir) of the model
type(Geometry_tp),    intent(out) :: Geometry   ! Holds information about the geometry of the domain

! Local Variables =================================================================================
! - integer Variables -----------------------------------------------------------------------------
integer(kind=Smll) :: UnFile   ! Holds Unit of a file for error message
integer(kind=Smll) :: IO_File  ! For IOSTAT: Input Output status in OPEN command
integer(kind=Smll) :: IO_read  ! Holds error of read statements
integer(kind=Smll) :: IO_write ! Used for IOSTAT - Input Output Status - in the write command.

! - real Variables --------------------------------------------------------------------------------
!#real(kind=Dbl)      ::
! - complex Variables -----------------------------------------------------------------------------
!#complex              ::
! - integer Arrays --------------------------------------------------------------------------------
!#integer(kind=Shrt), dimension(:)  ::
!#integer(kind=Shrt), allocatable, dimension(:)  ::
! - real Arrays -----------------------------------------------------------------------------------
!#real(kind=Dbl), dimension(:)      ::
!#real(kind=Dbl), allocatable, dimension(:)  ::
! - Character Variables ---------------------------------------------------------------------------
!#Character (kind = ?, Len = ? ) ::
! - Logical Variables -----------------------------------------------------------------------------
!#Logical   ::
! - Type ------------------------------------------------------------------------------------------

! code ============================================================================================

write(*,       *)
write(*,       *) " Subroutine < Input_Basic_sub >: "
write(FileInfo,*)
write(FileInfo,*) " Subroutine < Input_Basic_sub >: "

! - Opening the data model file -------------------------------------------------------------------
write(*,        fmt="(A)") " -Opening the data model file ..."
write(FileInfo, fmt="(A)") " -Opening the data model file ..."

print*,ModelInfo%ModelName
print*,ModelInfo%InputDir

UnFile=FileDataModel
open(Unit=UnFile, file=trim(ModelInfo%ModelName)//'.dataModel', &
     err=1001, iostat=IO_File, access='sequential', action='read', asynchronous='no', &
     blank='null', blocksize=0, defaultfile=trim(ModelInfo%InputDir), DisPOSE='keep', form='formatted', &
     position='asis', status='old')

UnFile = FileDataModel
read(unit=UnFile, fmt="(A)", advance='yes', asynchronous='no', iostat=IO_read, err=1003, end=1004)
read(unit=UnFile, fmt="(A)", advance='yes', asynchronous='no', iostat=IO_read, err=1003, end=1004)
read(unit=UnFile, fmt="(A)", advance='yes', asynchronous='no', iostat=IO_read, err=1003, end=1004)
read(unit=UnFile, fmt="(I10)", advance='yes', asynchronous='no', iostat=IO_read, err=1003, end=1004) Geometry%NoReaches
UnFile = FileInfo
write(unit=UnFile, fmt="(' Total number of reach(es) is(are): ', I10)", advance='yes', asynchronous='no', iostat=IO_write, err=1006) Geometry%NoReaches
write(unit=*,      fmt="(' Total number of reach(es) is(are): ', I10)") Geometry%NoReaches


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
!
! File version $Id $
!
! Last update: 02/23/2018
!
! ================================ L O C A L   V A R I A B L E S ==================================
! (Refer to the main code to see the list of imported variables)
!  . . . . . . . . . . . . . . . . Variables . . . . . . . . . . . . . . . . . . . . . . . . . . .
!
!##################################################################################################

Subroutine Input_Array_sub(                                           &
!                                                                     & ! integer(1) Variables
!                                                                     & ! integer(2) Variables
!                                                                     & ! integer(4) Variables
!                                                                     & ! integer(8) Variables
!                                                                     & ! real Variables
!                                                                     & ! integer Arrays
!                                                                     & ! real Arrays
!                                                                     & ! Characters
ModelInfo, Geometry                                                   & ! Type
)

! Libraries =======================================================================================

! User defined modules ============================================================================

Implicit None

! Global Variables ================================================================================

! - integer Variables -----------------------------------------------------------------------------
!#integer(kind=Shrt), intent(In)    ::
!#integer(kind=Shrt), intent(InOut) ::
!#integer(kind=Shrt), intent(OUT)   ::
! - real Variables --------------------------------------------------------------------------------
!#real(kind=Dbl),     intent(In)    ::
!#real(kind=Dbl),     intent(InOut) ::
!#real(kind=Dbl),     intent(OUT)   ::
! - complex Variables -----------------------------------------------------------------------------
!#complex,             intent(In)    ::
!#complex,             intent(InOut) ::
!#complex,             intent(OUT)   ::
! - integer Arrays --------------------------------------------------------------------------------
!#integer(kind=Shrt), intent(In),    dimension(:  )  ::
!#integer(kind=Shrt), intent(In),    dimension(:,:)  ::
!#integer(kind=Shrt), intent(In)    ::
!#integer(kind=Shrt), intent(InOut) ::
!#integer(kind=Shrt), intent(OUT)   ::
! - real Arrays -----------------------------------------------------------------------------------
!#real(kind=Dbl),     intent(In),    dimension(:  )  ::
!#real(kind=Dbl),     intent(InOut), dimension(:  )  ::
!#real(kind=Dbl),     intent(OUT),   dimension(:  )  ::
! - Character Variables ---------------------------------------------------------------------------
!#character (kind = ?, Len = ? ) ::
! - Logical Variables -----------------------------------------------------------------------------
!#logical   ::
! - types -----------------------------------------------------------------------------------------
type(Input_Data_tp),  intent(In) :: ModelInfo  ! Holds info. (name, dir, output dir) of the model
type(Geometry_tp),    intent(inout):: Geometry   ! Holds information about the geometry of the domain

! Local Variables =================================================================================
! - integer Variables -----------------------------------------------------------------------------
integer(kind=Smll) :: UnFile   ! Holds Unit of a file for error message
integer(kind=Smll) :: IO_File  ! For IOSTAT: Input Output status in OPEN command
integer(kind=Smll) :: IO_read  ! Holds error of read statements
integer(kind=Smll) :: IO_write ! Used for IOSTAT - Input Output Status - in the write command
integer(kind=Smll) :: i_reach  ! loop index on the number of reaches

! - real Variables --------------------------------------------------------------------------------
!#real(kind=Dbl)      ::
! - complex Variables -----------------------------------------------------------------------------
!#complex              ::
! - integer Arrays --------------------------------------------------------------------------------
!#integer(kind=Shrt), dimension(:) ::
!#integer(kind=Shrt), allocatable, dimension(:) ::
! - real Arrays -----------------------------------------------------------------------------------
!#real(kind=Dbl), dimension(:)      ::
!#real(kind=Dbl), allocatable, dimension(:)  ::
! - Character Variables ---------------------------------------------------------------------------
!#character (kind = ?, Len = ? ) ::
! - Logical Variables -----------------------------------------------------------------------------
!#logical   ::
! - Type ------------------------------------------------------------------------------------------

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
  do i_reach= 1, Geometry%NoReaches
    UnFile = FileDataGeo
    read(unit=UnFile, fmt="(F23.10)", advance='yes', asynchronous='no', iostat=IO_read, err=1003, end=1004) Geometry%ReachLength(i_reach); !write(*,*)Geometry%ReachLength(i_reach)
    UnFile = FileInfo
    write(unit=*,      fmt="(' The length of reach ', I5, ' is:', F23.10,' m')") i_reach, Geometry%ReachLength(i_reach)
    write(unit=UnFile, fmt="(' The length of reach ', I5, ' is:', F23.10,' m')") i_reach, Geometry%ReachLength(i_reach)
  end do

! Reading total number of control volumes in each reach/ For now we have a constant discretization in each reach.
UnFile = FileDataGeo
read(unit=UnFile, fmt="(A)", advance='yes', asynchronous='no', iostat=IO_read, err=1003, end=1004)
read(unit=UnFile, fmt="(A)", advance='yes', asynchronous='no', iostat=IO_read, err=1003, end=1004)
  do i_reach= 1, Geometry%NoReaches
    UnFile = FileDataGeo
    read(unit=UnFile, fmt="(I10)", advance='yes', asynchronous='no', iostat=IO_read, err=1003, end=1004) Geometry%ReachDisc(i_reach)
    UnFile = FileInfo
    write(unit=*,      fmt="(' No. of discretization of reach ', I5, ' is:', I10)") i_reach, Geometry%ReachDisc(i_reach)
    write(unit=UnFile, fmt="(' No. of discretization of reach ', I5, ' is:', I10)") i_reach, Geometry%ReachDisc(i_reach)
  end do

! Reading reach type
UnFile = FileDataGeo
read(unit=UnFile, fmt="(A)", advance='yes', asynchronous='no', iostat=IO_read, err=1003, end=1004)
read(unit=UnFile, fmt="(A)", advance='yes', asynchronous='no', iostat=IO_read, err=1003, end=1004)
  do i_reach= 1, Geometry%NoReaches
    UnFile = FileDataGeo
    read(unit=UnFile, fmt="(I10)", advance='yes', asynchronous='no', iostat=IO_read, err=1003, end=1004) Geometry%ReachType(i_reach)
    UnFile = FileInfo
    write(unit=*,      fmt="(' Rreach ', I10,' is of type: ', I5)") i_reach, Geometry%ReachType(i_reach)
    write(unit=UnFile, fmt="(' Rreach ', I10,' is of type: ', I5)") i_reach, Geometry%ReachType(i_reach)
  end do

! Reading slopes of reach
UnFile = FileDataGeo
read(unit=UnFile, fmt="(A)", advance='yes', asynchronous='no', iostat=IO_read, err=1003, end=1004)
read(unit=UnFile, fmt="(A)", advance='yes', asynchronous='no', iostat=IO_read, err=1003, end=1004)
  do i_reach= 1, Geometry%NoReaches
    UnFile = FileDataGeo
    read(unit=UnFile, fmt="(F23.10)", advance='yes', asynchronous='no', iostat=IO_read, err=1003, end=1004) Geometry%ReachSlope(i_reach)
    UnFile = FileInfo
    write(unit=*,      fmt="(' The slope of reach ', I10,' is: ', F23.10)") i_reach, Geometry%ReachSlope(i_reach)
    write(unit=UnFile, fmt="(' The slope of reach ', I10,' is: ', F23.10)") i_reach, Geometry%ReachSlope(i_reach)
  end do

! Reading Manning number of each reach
UnFile = FileDataGeo
read(unit=UnFile, fmt="(A)", advance='yes', asynchronous='no', iostat=IO_read, err=1003, end=1004)
read(unit=UnFile, fmt="(A)", advance='yes', asynchronous='no', iostat=IO_read, err=1003, end=1004)
  do i_reach= 1, Geometry%NoReaches
    UnFile = FileDataGeo
    read(unit=UnFile, fmt="(F23.10)", advance='yes', asynchronous='no', iostat=IO_read, err=1003, end=1004) Geometry%ReachManning(i_reach)
    UnFile = FileInfo
    write(unit=*,      fmt="(' The Mannings no. for reach ', I10,' is: ', F23.10)") i_reach, Geometry%ReachManning(i_reach)
    write(unit=UnFile, fmt="(' The Mannings no. for reach ', I10,' is: ', F23.10)") i_reach, Geometry%ReachManning(i_reach)
  end do

! Reading the width of each reach
UnFile = FileDataGeo
read(unit=UnFile, fmt="(A)", advance='yes', asynchronous='no', iostat=IO_read, err=1003, end=1004)
read(unit=UnFile, fmt="(A)", advance='yes', asynchronous='no', iostat=IO_read, err=1003, end=1004)
  do i_reach= 1, Geometry%NoReaches
    UnFile = FileDataGeo
    read(unit=UnFile, fmt="(F23.10)", advance='yes', asynchronous='no', iostat=IO_read, err=1003, end=1004) Geometry%ReachWidth(i_reach)
    UnFile = FileInfo
    write(unit=*,      fmt="(' The width of reach ', I10,'is: ', F23.10)") i_reach, Geometry%ReachWidth(i_reach)
    write(unit=UnFile, fmt="(' The width of reach ', I10,'is: ', F23.10)") i_reach, Geometry%ReachWidth(i_reach)
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

End Subroutine Input_Array_sub

!##################################################################################################
! Purpose: This subroutine reads information required for a particular analysis
!
! Developed by: Babak Poursartip
! Supervised by: Clint Dawson
!
! The Institute for Computational Engineering and Sciences (ICES)
! The University of Texas at Austin
!
! ================================ V E R S I O N ==================================================
! V0.1: 02/13/2018 - Initiation.
!
! File version $Id $
!
! Last update: 02/13/2018
!
! ================================ L O C A L   V A R I A B L E S ==================================
! (Refer to the main code to see the list of imported variables)
!  . . . . . . . . . . . . . . . . Variables . . . . . . . . . . . . . . . . . . . . . . . . . . .
!
!##################################################################################################

Subroutine Input_Analysis_sub(                                        &
!                                                                     & ! integer(1) Variables
!                                                                     & ! integer(2) Variables
i_analyses,                                                           & ! integer(4) Variables
!                                                                     & ! integer(8) Variables
!                                                                     & ! real Variables
!                                                                     & ! integer Arrays
!                                                                     & ! real Arrays
!                                                                     & ! Characters
ModelInfo, AnalysisInfo                                               & ! Type
)

! Libraries =======================================================================================
use ifport

! User defined modules ============================================================================

Implicit None

! Global Variables ================================================================================

! - integer Variables -----------------------------------------------------------------------------
integer(kind=Smll), intent(In) :: i_analyses

! - real Variables --------------------------------------------------------------------------------
!#real(kind=Dbl), intent(In)    ::
!#real(kind=Dbl), intent(InOut) ::
!#real(kind=Dbl), intent(OUT)   ::
! - complex Variables -----------------------------------------------------------------------------
!#complex, intent(In)    ::
!#complex, intent(InOut) ::
!#complex, intent(OUT)   ::
! - integer Arrays --------------------------------------------------------------------------------
!#integer(kind=Shrt), intent(In), dimension(:  )  ::
!#integer(kind=Shrt), intent(In), dimension(:,:)  ::
!#integer(kind=Shrt), intent(In)    ::
!#integer(kind=Shrt), intent(InOut) ::
!#integer(kind=Shrt), intent(OUT)   ::
! - real Arrays -----------------------------------------------------------------------------------
!#real(kind=Dbl),     intent(In),    dimension(:  )  ::
!#real(kind=Dbl),     intent(InOut), dimension(:  )  ::
!#real(kind=Dbl),     intent(OUT),   dimension(:  )  ::
! - Character Variables ---------------------------------------------------------------------------
!#Character (kind = ?, Len = ? ) ::
! - Logical Variables -----------------------------------------------------------------------------
!#Logical   ::
! - Types -----------------------------------------------------------------------------------------
type(Input_Data_tp), intent(inout) :: ModelInfo  ! Holds info. (name, dir, output dir) of the model, intent(In) :: ModelInfo  ! Holds info. (name, dir, output dir) of the model
type(AnalysisData_tp), intent(out) :: AnalysisInfo  ! Holds info. (name, dir, output dir) of the model, intent(In) :: ModelInfo  ! Holds info. (name, dir, output dir) of the model

! Local Variables =================================================================================
! - integer Variables -----------------------------------------------------------------------------
integer(kind=Smll) :: UnFile        ! Holds Unit of a file for error message
integer(kind=Smll) :: IO_File       ! For IOSTAT: Input Output status in OPEN command
integer(kind=Smll) :: IO_read  ! Holds error of read statements
integer(kind=Smll) :: IO_write ! Used for IOSTAT - Input Output Status - in the write command.

! - real Variables --------------------------------------------------------------------------------
!#real(kind=Dbl)      ::
! - complex Variables -----------------------------------------------------------------------------
!#complex              ::
! - integer Arrays --------------------------------------------------------------------------------
!#integer(kind=Shrt), dimension(:)  ::
!#integer(kind=Shrt), allocatable, dimension(:)  ::
! - real Arrays -----------------------------------------------------------------------------------
!#real(kind=Dbl), dimension(:)      ::
!#real(kind=Dbl), allocatable, dimension(:)  ::
! - Character Variables ---------------------------------------------------------------------------
!#Character (kind = ?, Len = ? ) ::
! - Logical Variables -----------------------------------------------------------------------------
logical(kind=Shrt)  :: Directory

! - Type ------------------------------------------------------------------------------------------


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
read(unit=UnFile, fmt="(I10)", advance='yes', asynchronous='no', iostat=IO_read, err=1003, end=1004) AnalysisInfo%AnalysisType
UnFile = FileInfo
write(unit=*,      fmt="(' The Analysis type is: ', I10)") AnalysisInfo%AnalysisType
write(unit=UnFile, fmt="(' The Analysis type is: ', I10)", advance='yes', asynchronous='no', iostat=IO_write, err=1006) AnalysisInfo%AnalysisType

UnFile = UnInptAna
read(unit=UnFile, fmt="(A)", advance='yes', asynchronous='no', iostat=IO_read, err=1003, end=1004)
read(unit=UnFile, fmt="(A)", advance='yes', asynchronous='no', iostat=IO_read, err=1003, end=1004)
read(unit=UnFile, fmt="(F23.10)", advance='yes', asynchronous='no', iostat=IO_read, err=1003, end=1004) AnalysisInfo%TotalTime
UnFile = FileInfo
write(unit=*,      fmt="(' The total simulation time is: ', F23.10, ' s')") AnalysisInfo%TotalTime
write(unit=UnFile, fmt="(' The total simulation time is: ', F23.10, ' s')", advance='yes', asynchronous='no', iostat=IO_write, err=1006) AnalysisInfo%TotalTime

UnFile = UnInptAna
read(unit=UnFile, fmt="(A)", advance='yes', asynchronous='no', iostat=IO_read, err=1003, end=1004)
read(unit=UnFile, fmt="(A)", advance='yes', asynchronous='no', iostat=IO_read, err=1003, end=1004)
read(unit=UnFile, fmt="(F23.10)", advance='yes', asynchronous='no', iostat=IO_read, err=1003, end=1004) AnalysisInfo%TimeStep
UnFile = FileInfo
write(unit=UnFile, fmt="(' The time step is: ', F23.10, ' s')", advance='yes', asynchronous='no', iostat=IO_write, err=1006) AnalysisInfo%TimeStep
write(unit=*,      fmt="(' The time step is: ', F23.10, ' s')") AnalysisInfo%TimeStep

UnFile = UnInptAna
read(unit=UnFile, fmt="(A)", advance='yes', asynchronous='no', iostat=IO_read, err=1003, end=1004)
read(unit=UnFile, fmt="(A)", advance='yes', asynchronous='no', iostat=IO_read, err=1003, end=1004)
read(unit=UnFile, fmt="(F23.10)", advance='yes', asynchronous='no', iostat=IO_read, err=1003, end=1004) AnalysisInfo%Q_Up
UnFile = FileInfo
write(unit=UnFile, fmt="(' Flow rate at the upstream is: ', F23.10, ' m/s3')", advance='yes', asynchronous='no', iostat=IO_write, err=1006) AnalysisInfo%Q_Up
write(unit=*,      fmt="(' Flow rate at the upstream is: ', F23.10, ' m/s3')") AnalysisInfo%Q_Up

UnFile = UnInptAna
read(unit=UnFile, fmt="(A)", advance='yes', asynchronous='no', iostat=IO_read, err=1003, end=1004)
read(unit=UnFile, fmt="(A)", advance='yes', asynchronous='no', iostat=IO_read, err=1003, end=1004)
read(unit=UnFile, fmt="(F23.10)", advance='yes', asynchronous='no', iostat=IO_read, err=1003, end=1004) AnalysisInfo%h_dw
UnFile = FileInfo
write(unit=UnFile, fmt="(' Downstream water depth is: ', F23.10, ' m')", advance='yes', asynchronous='no', iostat=IO_write, err=1006) AnalysisInfo%h_dw
write(unit=*,      fmt="(' Downstream water depth is: ', F23.10, ' m')") AnalysisInfo%h_dw

UnFile = UnInptAna
read(unit=UnFile, fmt="(A)", advance='yes', asynchronous='no', iostat=IO_read, err=1003, end=1004)
read(unit=UnFile, fmt="(A)", advance='yes', asynchronous='no', iostat=IO_read, err=1003, end=1004)
read(unit=UnFile, fmt="(F23.10)", advance='yes', asynchronous='no', iostat=IO_read, err=1003, end=1004) AnalysisInfo%CntrlV
UnFile = FileInfo
write(unit=UnFile, fmt="(' Control Volume is: ', F23.10, ' m^3')", advance='yes', asynchronous='no', iostat=IO_write, err=1006) AnalysisInfo%CntrlV
write(unit=*,      fmt="(' Control Volume is: ', F23.10, ' m^3')") AnalysisInfo%CntrlV

UnFile = UnInptAna
read(unit=UnFile, fmt="(A)", advance='yes', asynchronous='no', iostat=IO_read, err=1003, end=1004)
read(unit=UnFile, fmt="(A)", advance='yes', asynchronous='no', iostat=IO_read, err=1003, end=1004)
read(unit=UnFile, fmt="(F23.10)", advance='yes', asynchronous='no', iostat=IO_read, err=1003, end=1004) AnalysisInfo%CntrlV_ratio
UnFile = FileInfo
write(unit=UnFile, fmt="(' The ratio of control volume is: ', F23.10)", advance='yes', asynchronous='no', iostat=IO_write, err=1006) AnalysisInfo%CntrlV_ratio
write(unit=*,      fmt="(' The ratio of control volume is: ', F23.10)") AnalysisInfo%CntrlV_ratio

UnFile = UnInptAna
read(unit=UnFile, fmt="(A)", advance='yes', asynchronous='no', iostat=IO_read, err=1003, end=1004)
read(unit=UnFile, fmt="(A)", advance='yes', asynchronous='no', iostat=IO_read, err=1003, end=1004)
read(unit=UnFile, fmt="(I10)", advance='yes', asynchronous='no', iostat=IO_read, err=1003, end=1004) AnalysisInfo%limiter
UnFile = FileInfo
write(unit=UnFile, fmt="(' The limiter is: ', I10)", advance='yes', asynchronous='no', iostat=IO_write, err=1006) AnalysisInfo%limiter
write(unit=*,      fmt="(' The limiter is: ', I10)") AnalysisInfo%CntrlV_ratio



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
