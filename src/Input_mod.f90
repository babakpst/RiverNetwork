
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
! V0.1: 02/13/2018 - Initiation.
!
! File version $Id $
!
! Last update: 13/02/2018
!
! ================================ S U B R O U T I N E ============================================
!
!
! ================================ F U N C T I O N ================================================
!
! ================================ L O C A L   V A R I A B L E S ==================================
! (Refer to the main code to see the list of imported variables)
!  . . . . . . . . . . . . . . . . Variables . . . . . . . . . . . . . . . . . . . . . . . . . . .
!
!##################################################################################################

module Input_mod

use Parameters

implicit none


interface Input
  module procedure Input_Address_sub, Input_Basic_sub, Input_Array_sub, Input_Analysis_sub
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
! V0.1: 02/13/2018 - Initiation.
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
!#integer(Kind=Shrt), intent(In)    ::
!#integer(Kind=Shrt), intent(InOut) ::
!#integer(Kind=Shrt), intent(OUT)   ::
! - real Variables --------------------------------------------------------------------------------
!#real(Kind=Dbl),     intent(In)    ::
!#real(Kind=Dbl),     intent(InOut) ::
!#real(Kind=Dbl),     intent(OUT)   ::
! - complex Variables -----------------------------------------------------------------------------
!#complex,             intent(In)    ::
!#complex,             intent(InOut) ::
!#complex,             intent(OUT)   ::
! - integer Arrays --------------------------------------------------------------------------------
!#integer(Kind=Shrt), intent(In),    dimension(:  )  ::
!#integer(Kind=Shrt), intent(In),    dimension(:,:)  ::
!#integer(Kind=Shrt), intent(In)    ::
!#integer(Kind=Shrt), intent(InOut) ::
!#integer(Kind=Shrt), intent(OUT)   ::
! - real Arrays -----------------------------------------------------------------------------------
!#real(Kind=Dbl),     intent(In),    dimension(:  )  ::
!#real(Kind=Dbl),     intent(InOut), dimension(:  )  ::
!#real(Kind=Dbl),     intent(OUT),   dimension(:  )  ::
! - Character Variables ---------------------------------------------------------------------------
!#Character (Kind = ?, Len = ? ) ::
! - Logical Variables -----------------------------------------------------------------------------
!#Logical   ::
! - Types -----------------------------------------------------------------------------------------
type(Input_Data_tp), intent(out) :: ModelInfo  ! Holds info. (name, dir, output dir) of the model :: ModelInfo  ! Holds info. (name, dir, output dir) of the model

! Local Variables =================================================================================
! - integer Variables -----------------------------------------------------------------------------
integer(Kind=Smll) :: UnFile        ! Holds Unit of a file for error message
integer(Kind=Smll) :: IO_File       ! For IOSTAT: Input Output status in OPEN command
integer(Kind=Smll) :: i_analyses    ! loop index to read the analyses files
integer(Kind=Smll) :: ERR_Alloc, ERR_DeAlloc ! Allocating and DeAllocating errors

! - real Variables --------------------------------------------------------------------------------
!#real(Kind=Dbl)      ::
! - complex Variables -----------------------------------------------------------------------------
!#complex              ::
! - integer Arrays --------------------------------------------------------------------------------
!#integer(Kind=Shrt), dimension(:)  ::
!#integer(Kind=Shrt), allocatable, dimension(:)  ::
! - real Arrays -----------------------------------------------------------------------------------
!#real(Kind=Dbl), dimension(:)      ::
!#real(Kind=Dbl), allocatable, dimension(:)  ::
! - Character Variables ---------------------------------------------------------------------------
!#Character (Kind = ?, Len = ? ) ::
! - Logical Variables -----------------------------------------------------------------------------
Logical (Kind=Shrt)  :: Directory

! - Type ------------------------------------------------------------------------------------------

! code ============================================================================================

write(*,       *) " Subroutine < Input_Address_sub >: "
write(FileInfo,*) " Subroutine < Input_Address_sub >: "

UnFile=FileAdr
Open(Unit=UnFile, File='Address.txt', Err=1001, IOStat=IO_File, Access='SEQUENTIAL', &
     action='READ', Asynchronous='NO', blank='NULL', blocksize=0, DisPOSE='Keep', &
     Form='formatted', position='ASIS', status='old')

! Read the input fine name and directories Form "ADDRESS_File.txt" in the current directory -------
read(FileAdr,*)
read(FileAdr,*) ModelInfo%ModelName
read(FileAdr,*)
read(FileAdr,*)
read(FileAdr,*) ModelInfo%AnalysisType
read(FileAdr,*)
read(FileAdr,*)
read(FileAdr,*) ModelInfo%InputDir
read(FileAdr,*)
read(FileAdr,*)
read(FileAdr,*) ModelInfo%OutputDir
read(FileAdr,*)
read(FileAdr,*)
read(FileAdr,*) ModelInfo%NumberOfAnalyses

! Allocating
Allocate (ModelInfo%AnalysesNames(ModelInfo%NumberOfAnalyses),  STAT = ERR_Alloc)
  If ( ERR_Alloc /= 0 ) Then
    write (*, Fmt_ALLCT) ERR_Alloc ;  write (FileInfo, Fmt_ALLCT) ERR_Alloc ;
    write(*, Fmt_FL) ;  write(FileInfo, Fmt_FL) ; read(*, Fmt_End) ;  stop ;
  End If
read(FileAdr,*)
  do i_analyses = 1, ModelInfo%NumberOfAnalyses
    read(FileAdr,*) ModelInfo%AnalysesNames(i_analyses)
  end do

ModelInfo%AnalysisDir=trim(AdjustL(ModelInfo%InputDir))//'/'//trim(AdjustL(ModelInfo%ModelName))//'/'//'Analysis'
ModelInfo%InputDir=trim(AdjustL(ModelInfo%InputDir))//'/'//trim(AdjustL(ModelInfo%ModelName))//'/'//'Model'

write (*, fmt="(2A)")" The model directory is: ", ModelInfo%InputDir
write (*, fmt="(2A)")" The analysis name is: ", ModelInfo%AnalysisDir

! Create the results folder
write(*,fmt="(A)") " -Creating the output folders ..."

Directory=MakeDirQQ (trim(AdjustL(ModelInfo%OutputDir))//'/'//trim(AdjustL(ModelInfo%ModelName)))
  If (Directory) Then
    write(*,fmt="(A)") "The result folder is created."
  Else
     write (*,fmt="(A)") "The result folder already exists."
  End If

ModelInfo%OutputDir=trim(AdjustL (ModelInfo%OutputDir))//'/'//trim(AdjustL (ModelInfo%ModelName))

write (*,fmt="(2A)")" The output directory is: ", ModelInfo%OutputDir

! - Closing the address file ----------------------------------------------------------------------
write(*,        fmt="(A)") " -Closing the address file"
write(FileInfo, fmt="(A)") " -Closing the address file"
UnFile =  FileAdr
Close(unit = UnFile, status = 'KEEP', ERR =  1002, IOSTAT = IO_File)


write(*,       *) 'End Subroutine < Input_Address_sub >'
write(FileInfo,*) 'End Subroutine < Input_Address_sub >'
Return

! Errors ==========================================================================================
! Opening statement Errors
1001 If (IO_File > 0) Then
       write(*, Fmt_Err1_OPEN) UnFile, IO_File; write(FileInfo, Fmt_Err1_OPEN) UnFile, IO_File;
       write(*, Fmt_FL); write(FileInfo, Fmt_FL);
       write(*, Fmt_End); read(*,*); stop;
     Else If ( IO_File < 0 ) Then
       write(*, Fmt_Err1_OPEN) UnFile, IO_File
       write(FileInfo, Fmt_Err1_OPEN) UnFile, IO_File;  write(*, Fmt_FL) ; write(FileInfo, Fmt_FL);
       write(*, Fmt_End); read(*,*); stop;
     End If


! Close statement Errors
1002 If (IO_File > 0) Then
       write(*, Fmt_Err1_Close) UnFile, IO_File; write(FileInfo, Fmt_Err1_Close) UnFile, IO_File;
       write(*, Fmt_FL); write(FileInfo, Fmt_FL);
       write(*, Fmt_End); read(*,*); stop;
     End If


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

Subroutine Input_Basic_sub(                                           &
!                                                                     & ! integer(1) Variables
!                                                                     & ! integer(2) Variables
!                                                                     & ! integer(4) Variables
!                                                                     & ! integer(8) Variables
!                                                                     & ! real Variables
!                                                                     & ! integer Arrays
!                                                                     & ! real Arrays
!                                                                     & ! Characters
ModelInfo, InitialInfo                                                & ! Type
)

! Libraries =======================================================================================

! User defined modules ============================================================================

Implicit None

! Global Variables ================================================================================

! - integer Variables -----------------------------------------------------------------------------
!#integer(Kind=Shrt), intent(In)    ::
!#integer(Kind=Shrt), intent(InOut) ::
!#integer(Kind=Shrt), intent(OUT)   ::
! - real Variables --------------------------------------------------------------------------------
!#real(Kind=Dbl),     intent(In)    ::
!#real(Kind=Dbl),     intent(InOut) ::
!#real(Kind=Dbl),     intent(OUT)   ::
! - complex Variables -----------------------------------------------------------------------------
!#complex,             intent(In)    ::
!#complex,             intent(InOut) ::
!#complex,             intent(OUT)   ::
! - integer Arrays --------------------------------------------------------------------------------
!#integer(Kind=Shrt), intent(In),    dimension(:  )  ::
!#integer(Kind=Shrt), intent(In),    dimension(:,:)  ::
!#integer(Kind=Shrt), intent(In)    ::
!#integer(Kind=Shrt), intent(InOut) ::
!#integer(Kind=Shrt), intent(OUT)   ::
! - real Arrays -----------------------------------------------------------------------------------
!#real(Kind=Dbl),     intent(In),    dimension(:  )  ::
!#real(Kind=Dbl),     intent(InOut), dimension(:  )  ::
!#real(Kind=Dbl),     intent(OUT),   dimension(:  )  ::
! - Character Variables ---------------------------------------------------------------------------
!#Character (Kind = ?, Len = ? ) ::
! - Logical Variables -----------------------------------------------------------------------------
!#Logical   ::
! - Types -----------------------------------------------------------------------------------------
type(Input_Data_tp),  intent(In)  :: ModelInfo   ! Holds info. (name, dir, output dir) of the model :: ModelInfo  ! Holds info. (name, dir, output dir) of the model
type(InitialData_tp), intent(out) :: InitialInfo ! Holds initial data required for array allocation

! Local Variables =================================================================================
! - integer Variables -----------------------------------------------------------------------------
integer(Kind=Smll) :: UnFile   ! Holds Unit of a file for error message
integer(Kind=Smll) :: IO_File  ! For IOSTAT: Input Output status in OPEN command
integer(Kind=Smll) :: IO_read  ! Holds error of read statements

! - real Variables --------------------------------------------------------------------------------
!#real(Kind=Dbl)      ::
! - complex Variables -----------------------------------------------------------------------------
!#complex              ::
! - integer Arrays --------------------------------------------------------------------------------
!#integer(Kind=Shrt), dimension(:)  ::
!#integer(Kind=Shrt), allocatable, dimension(:)  ::
! - real Arrays -----------------------------------------------------------------------------------
!#real(Kind=Dbl), dimension(:)      ::
!#real(Kind=Dbl), allocatable, dimension(:)  ::
! - Character Variables ---------------------------------------------------------------------------
!#Character (Kind = ?, Len = ? ) ::
! - Logical Variables -----------------------------------------------------------------------------
!#Logical   ::
! - Type ------------------------------------------------------------------------------------------

! code ============================================================================================

write(*,       *) " Subroutine < Input_Basic_sub >: "
write(FileInfo,*) " Subroutine < Input_Basic_sub >: "

! - Opening the data model file -------------------------------------------------------------------
write(*,        fmt="(A)") " -Opening the data model file ..."
write(FileInfo, fmt="(A)") " -Opening the data model file ..."

UnFile=FileDataModel
open(Unit=UnFile, file=trim(ModelInfo%ModelName)//'.dataModel', &
     err=1001, iostat=IO_File, access='sequential', action='read', asynchronous='no', &
     blank='null', blocksize=0, defaultfile=trim(ModelInfo%InputDir), DisPOSE='keep', form='formatted', &
     position='asis', status='old')














! - Closing the data model file -------------------------------------------------------------------
write(*,        fmt="(A)") " -Closing the data model file"
write(FileInfo, fmt="(A)") " -Closing the data model file"

UnFile =  FileDataModel
Close(Unit = UnFile, status = 'KEEP', ERR =  1002, IOSTAT = IO_File)

write(*,       *) 'End Subroutine < Input_Basic_sub >'
write(FileInfo,*) 'End Subroutine < Input_Basic_sub >'
Return

! Errors ==========================================================================================
! Opening statement Errors
1001 If (IO_File > 0) Then
       write(*, Fmt_Err1_OPEN) UnFile, IO_File; write(FileInfo, Fmt_Err1_OPEN) UnFile, IO_File;
       write(*, Fmt_FL); write(FileInfo, Fmt_FL);
       write(*, Fmt_End); read(*,*); stop;
     Else If ( IO_File < 0 ) Then
       write(*, Fmt_Err1_OPEN) UnFile, IO_File
       write(FileInfo, Fmt_Err1_OPEN) UnFile, IO_File;  write(*, Fmt_FL) ; write(FileInfo, Fmt_FL);
       write(*, Fmt_End); read(*,*); stop;
     End If


! Close statement Errors
1002 If (IO_File > 0) Then
       write(*, Fmt_Err1_Close) UnFile, IO_File; write(FileInfo, Fmt_Err1_Close) UnFile, IO_File;
       write(*, Fmt_FL); write(FileInfo, Fmt_FL);
       write(*, Fmt_End); read(*,*); stop;
     End If

! - Error in read statement -----------------------------------------------------------------------
1003 write(*, Fmt_read1) UnFile, IO_read; write(UnFile, Fmt_read1) UnFile, IO_read;
     write(*, Fmt_FL);  write(FileInfo, Fmt_FL); write(*, Fmt_End); read(*,*);  stop;

! - End-OF-FILE in read statement -----------------------------------------------------------------
1004 write(*, Fmt_read2) UnFile, IO_read; write(UnFile, Fmt_read2) UnFile, IO_read;
     write(*, Fmt_FL);  write(FileInfo, Fmt_FL); write(*, Fmt_End); read(*,*);  stop;

! - End-OF-FILE IN read STATEMENT -----------------------------------------------------------------
1005 write(*, Fmt_read3) UnFile, IO_read; write(UnFile, Fmt_read3) UnFile, IO_read;
     write(*, Fmt_FL);  write(FileInfo, Fmt_FL); write(*, Fmt_End); read(*,*);  stop;

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

Subroutine Input_Array_sub(                                                       &
!                                                                     & ! integer(1) Variables
!                                                                     & ! integer(2) Variables
!                                                                     & ! integer(4) Variables
!                                                                     & ! integer(8) Variables
!                                                                     & ! real Variables
!                                                                     & ! integer Arrays
!                                                                     & ! real Arrays
!                                                                     & ! Characters
!                                                                     & ! Type
)

! Libraries =======================================================================================

! User defined modules ============================================================================

Implicit None

! Global Variables ================================================================================

! - integer Variables -----------------------------------------------------------------------------
!#integer(Kind=Shrt), intent(In)    ::
!#integer(Kind=Shrt), intent(InOut) ::
!#integer(Kind=Shrt), intent(OUT)   ::
! - real Variables --------------------------------------------------------------------------------
!#real(Kind=Dbl),     intent(In)    ::
!#real(Kind=Dbl),     intent(InOut) ::
!#real(Kind=Dbl),     intent(OUT)   ::
! - complex Variables -----------------------------------------------------------------------------
!#complex,             intent(In)    ::
!#complex,             intent(InOut) ::
!#complex,             intent(OUT)   ::
! - integer Arrays --------------------------------------------------------------------------------
!#integer(Kind=Shrt), intent(In),    dimension(:  )  ::
!#integer(Kind=Shrt), intent(In),    dimension(:,:)  ::
!#integer(Kind=Shrt), intent(In)    ::
!#integer(Kind=Shrt), intent(InOut) ::
!#integer(Kind=Shrt), intent(OUT)   ::
! - real Arrays -----------------------------------------------------------------------------------
!#real(Kind=Dbl),     intent(In),    dimension(:  )  ::
!#real(Kind=Dbl),     intent(InOut), dimension(:  )  ::
!#real(Kind=Dbl),     intent(OUT),   dimension(:  )  ::
! - Character Variables ---------------------------------------------------------------------------
!#Character (Kind = ?, Len = ? ) ::
! - Logical Variables -----------------------------------------------------------------------------
!#Logical   ::
! - Types -----------------------------------------------------------------------------------------
!#type() ::

! Local Variables =================================================================================
! - integer Variables -----------------------------------------------------------------------------
!#integer(Kind=Shrt)  ::
! - real Variables --------------------------------------------------------------------------------
!#real(Kind=Dbl)      ::
! - complex Variables -----------------------------------------------------------------------------
!#complex              ::
! - integer Arrays --------------------------------------------------------------------------------
!#integer(Kind=Shrt), dimension(:)  ::
!#integer(Kind=Shrt), allocatable, dimension(:)  ::
! - real Arrays -----------------------------------------------------------------------------------
!#real(Kind=Dbl), dimension(:)      ::
!#real(Kind=Dbl), allocatable, dimension(:)  ::
! - Character Variables ---------------------------------------------------------------------------
!#Character (Kind = ?, Len = ? ) ::
! - Logical Variables -----------------------------------------------------------------------------
!#Logical   ::
! - Type ------------------------------------------------------------------------------------------

! code ============================================================================================

write(*,       *) " Subroutine < Input_Array_sub >: "
write(FileInfo,*) " Subroutine < Input_Array_sub >: "


write(*,       *) "End Subroutine < Input_Array_sub >"
write(FileInfo,*) "End Subroutine < Input_Array_sub >"
Return
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
i_analysis,                                                           & ! integer(4) Variables
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
integer(Kind=Smll), intent(In) :: i_analysis

! - real Variables --------------------------------------------------------------------------------
!#real(Kind=Dbl), intent(In)    ::
!#real(Kind=Dbl), intent(InOut) ::
!#real(Kind=Dbl), intent(OUT)   ::
! - complex Variables -----------------------------------------------------------------------------
!#complex, intent(In)    ::
!#complex, intent(InOut) ::
!#complex, intent(OUT)   ::
! - integer Arrays --------------------------------------------------------------------------------
!#integer(Kind=Shrt), intent(In), dimension(:  )  ::
!#integer(Kind=Shrt), intent(In), dimension(:,:)  ::
!#integer(Kind=Shrt), intent(In)    ::
!#integer(Kind=Shrt), intent(InOut) ::
!#integer(Kind=Shrt), intent(OUT)   ::
! - real Arrays -----------------------------------------------------------------------------------
!#real(Kind=Dbl),     intent(In),    dimension(:  )  ::
!#real(Kind=Dbl),     intent(InOut), dimension(:  )  ::
!#real(Kind=Dbl),     intent(OUT),   dimension(:  )  ::
! - Character Variables ---------------------------------------------------------------------------
!#Character (Kind = ?, Len = ? ) ::
! - Logical Variables -----------------------------------------------------------------------------
!#Logical   ::
! - Types -----------------------------------------------------------------------------------------
type(Input_Data_tp), intent(inout) :: ModelInfo  ! Holds info. (name, dir, output dir) of the model, intent(In) :: ModelInfo  ! Holds info. (name, dir, output dir) of the model

! Local Variables =================================================================================
! - integer Variables -----------------------------------------------------------------------------
integer(Kind=Smll) :: UnFile        ! Holds Unit of a file for error message
integer(Kind=Smll) :: IO_File       ! For IOSTAT: Input Output status in OPEN command

! - real Variables --------------------------------------------------------------------------------
!#real(Kind=Dbl)      ::
! - complex Variables -----------------------------------------------------------------------------
!#complex              ::
! - integer Arrays --------------------------------------------------------------------------------
!#integer(Kind=Shrt), dimension(:)  ::
!#integer(Kind=Shrt), allocatable, dimension(:)  ::
! - real Arrays -----------------------------------------------------------------------------------
!#real(Kind=Dbl), dimension(:)      ::
!#real(Kind=Dbl), allocatable, dimension(:)  ::
! - Character Variables ---------------------------------------------------------------------------
!#Character (Kind = ?, Len = ? ) ::
! - Logical Variables -----------------------------------------------------------------------------
logical(Kind=Shrt)  :: Directory

! - Type ------------------------------------------------------------------------------------------


! code ============================================================================================
write(*,       *) " Subroutine < Input_Analysis_sub >: "
write(FileInfo,*) " Subroutine < Input_Analysis_sub >: "


! Opening the input file for this specific simulation
write (*,        fmt="(A)") " -Opening the analysis file ..."
write (FileInfo, fmt="(A)") " -Opening the analysis file ..."

UnFile=UnInptAna
Open(Unit=UnFile, File=trim(ModelInfo%AnalysesNames(i_analysis))//'.txt', Err= 1001, IOStat=IO_File, Access='SEQUENTIAL', &
      action='READ', Asynchronous='NO', blank='NULL', blocksize=0, defaultfile=trim(ModelInfo%AnalysisDir), &
      dispose='Keep', form='formatted', position='ASIS', status='old') ;

! Creating the output file directory for this analysis --------------------------------------------
write(*,        fmt="(A)") " -Creating the output folder for this analysis ..."
write(FileInfo, fmt="(A)") " -Creating the output folder for this analysis ..."

Directory=MakeDirQQ (trim(AdjustL (ModelInfo%OutputDir))//'/'//trim(AdjustL(ModelInfo%AnalysesNames(i_analysis))))
  If (Directory) Then ;
     write (*,       fmt="(A)") "The output folder for this analysis created." ;
     write (FileInfo,fmt="(A)") "The output folder for this analysis created." ;
  Else ;
     write (*,        fmt="(A)") "The output folder for this analysis already exists." ;
     write (FileInfo, fmt="(A)") "The output folder for this analysis already exists." ;
  End If ;

ModelInfo%AnalysisOutputDir=trim(AdjustL(ModelInfo%OutputDir))//'/'//trim(AdjustL(ModelInfo%AnalysesNames(i_analysis)))

write(*,       *) "End Subroutine < Input_Analysis_sub >"
write(FileInfo,*) "End Subroutine < Input_Analysis_sub >"
Return

! Errors ==========================================================================================
! Opening statement Errors
1001 If (IO_File > 0) Then
       write(*, Fmt_Err1_OPEN) UnFile, IO_File; write(FileInfo, Fmt_Err1_OPEN) UnFile, IO_File;
       write(*, Fmt_FL); write(FileInfo, Fmt_FL);
       write(*, Fmt_End); read(*,*); stop;
     Else If ( IO_File < 0 ) Then
       write(*, Fmt_Err1_OPEN) UnFile, IO_File
       write(FileInfo, Fmt_Err1_OPEN) UnFile, IO_File;  write(*, Fmt_FL) ; write(FileInfo, Fmt_FL);
       write(*, Fmt_End); read(*,*); stop;
     End If


! Close statement Errors
1002 If (IO_File > 0) Then
       write(*, Fmt_Err1_Close) UnFile, IO_File; write(FileInfo, Fmt_Err1_Close) UnFile, IO_File;
       write(*, Fmt_FL); write(FileInfo, Fmt_FL);
       write(*, Fmt_End); read(*,*); stop;
     End If

End Subroutine Input_Analysis_sub


end module Input_mod
