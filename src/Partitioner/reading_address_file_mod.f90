
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
! V2.10: 05/15/2018 - Separating the input from model
! V2.20: 05/30/2018 - Initializing types
!
! File version $Id $
!
! Last update: 05/30/2018
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
  character (kind = 1, Len = 30 ) :: ModelName=" "     ! Name of the model input file
  character (kind = 1, Len = 150) :: InputDir=" "      ! Directory of the input file.
  character (kind = 1, Len = 150) :: AnalysisDir=" "   ! Directory of Analysis input file.
  character (kind = 1, Len = 150) :: OutputDir=" "     ! Directory of output files (Results)
  character (kind = 1, Len = 150) :: AnalysisOutputDir=" "! Dir. of output file for each analysis
  character (kind = 1, Len = 150), dimension(:), allocatable :: AnalysesNames! Holds the names of
                                                                        ! the analysis input files
  !integer(kind=Smll):: OutputType        ! Output Type: 1: ordinary-2: HDF5
  integer(kind=Smll) :: NumberOfAnalyses=0_smll  ! Number of analysis

  real(kind=SGL) :: Version=0.0               ! Holds the version of the code.

  contains
    procedure Input => Input_Address_sub

end type Input_Data_tp

public:: Input_Data_tp

contains


!##################################################################################################
! Purpose: This subroutine/function reads the general information about the name of the simulation
!           model, directories, etc.
!
! Developed by:  Babak Poursartip
! Supervised by: Clint Dawson
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
read(FileAdr,*) this%InputDir; !write(*,*) this%InputDir
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

this%AnalysisDir=trim(AdjustL(this%InputDir))//'/'// &
                      trim(AdjustL(this%ModelName))//'/'//'Analysis'
this%InputDir   =trim(AdjustL(this%InputDir))//'/'// &
                      trim(AdjustL(this%ModelName))//'/'//'Model'

write(*, fmt="(2A)")" The model directory is: ", this%InputDir
write(*, fmt="(2A)")" The analysis name is: ", this%AnalysisDir
write(*, fmt="(2A)")" The input file name is: ", this%ModelName

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

end module Input_mod
