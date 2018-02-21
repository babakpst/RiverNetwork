
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
!                                                                     & ! integer (1) Variables
!                                                                     & ! integer (2) Variables
!                                                                     & ! integer (4) Variables
!                                                                     & ! integer (8) Variables
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
!#integer (Kind=Shrt), Intent(In)    ::
!#integer (Kind=Shrt), Intent(InOut) ::
!#integer (Kind=Shrt), Intent(OUT)   ::
! - real Variables --------------------------------------------------------------------------------
!#real (Kind=Dbl),     Intent(In)    ::
!#real (Kind=Dbl),     Intent(InOut) ::
!#real (Kind=Dbl),     Intent(OUT)   ::
! - complex Variables -----------------------------------------------------------------------------
!#complex,             Intent(In)    ::
!#complex,             Intent(InOut) ::
!#complex,             Intent(OUT)   ::
! - integer Arrays --------------------------------------------------------------------------------
!#integer (Kind=Shrt), Intent(In),    Dimension (:  )  ::
!#integer (Kind=Shrt), Intent(In),    Dimension (:,:)  ::
!#integer (Kind=Shrt), Intent(In)    ::
!#integer (Kind=Shrt), Intent(InOut) ::
!#integer (Kind=Shrt), Intent(OUT)   ::
! - real Arrays -----------------------------------------------------------------------------------
!#real (Kind=Dbl),     Intent(In),    Dimension (:  )  ::
!#real (Kind=Dbl),     Intent(InOut), Dimension (:  )  ::
!#real (Kind=Dbl),     Intent(OUT),   Dimension (:  )  ::
! - Character Variables ---------------------------------------------------------------------------
!#Character (Kind = ?, Len = ? ) ::
! - Logical Variables -----------------------------------------------------------------------------
!#Logical   ::
! - Types -----------------------------------------------------------------------------------------
type (Input_Data_tp), Intent(out) :: ModelInfo  ! Holds info. (name, dir, output dir) of the model :: ModelInfo  ! Holds info. (name, dir, output dir) of the model


! Local Variables =================================================================================
! - integer Variables -----------------------------------------------------------------------------
integer (Kind=Smll) :: UnFile        ! Holds Unit of a file for error message
integer (Kind=Smll) :: IO_File       ! For IOSTAT: Input Output Status in OPEN command
integer (Kind=Smll) :: i_analyses    ! loop index to read the analyses files
integer (Kind=Smll) :: ERR_Alloc, ERR_DeAlloc ! Allocating and DeAllocating errors

! - real Variables --------------------------------------------------------------------------------
!#real (Kind=Dbl)      ::
! - complex Variables -----------------------------------------------------------------------------
!#complex              ::
! - integer Arrays --------------------------------------------------------------------------------
!#integer (Kind=Shrt), Dimension (:)  ::
!#integer (Kind=Shrt), Allocatable, Dimension (:)  ::
! - real Arrays -----------------------------------------------------------------------------------
!#real (Kind=Dbl), Dimension (:)      ::
!#real (Kind=Dbl), Allocatable, Dimension (:)  ::
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
     Action='READ', Asynchronous='NO', Blank='NULL', BLOCKSize=0, DisPOSE='Keep', &
     Form='FormATTED', Position='ASIS', Status='old')

! Read the input fine name and directories Form "ADDRESS_File.txt" in the current directory -------
Read(FileAdr,*)
Read(FileAdr,*) ModelInfo%ModelName
Read(FileAdr,*)
Read(FileAdr,*)
Read(FileAdr,*) ModelInfo%AnalysisType
Read(FileAdr,*)
Read(FileAdr,*)
Read(FileAdr,*) ModelInfo%InputDir
Read(FileAdr,*)
Read(FileAdr,*)
Read(FileAdr,*) ModelInfo%IntDir
Read(FileAdr,*)
Read(FileAdr,*)
Read(FileAdr,*) ModelInfo%OutputDir
Read(FileAdr,*)
Read(FileAdr,*)
Read(FileAdr,*) ModelInfo%NumberOfAnalyses

! Allocating
Allocate (ModelInfo%AnalysesNames(ModelInfo%NumberOfAnalyses),  STAT = ERR_Alloc)
  If ( ERR_Alloc /= 0 ) Then
    Write (*, Fmt_ALLCT) ERR_Alloc ;  Write (FileInfo, Fmt_ALLCT) ERR_Alloc ;
    Write(*, Fmt_FL) ;  Write(FileInfo, Fmt_FL) ; Read(*, Fmt_End) ;  Stop ;
  End If
Read(FileAdr,*)
  do i_analyses = 1, ModelInfo%NumberOfAnalyses
    Read(FileAdr,*) ModelInfo%AnalysesNames(i_analyses)
  end do

ModelInfo%AnalysisDir=Trim(AdjustL(ModelInfo%InputDir))//'/'//Trim(AdjustL(ModelInfo%ModelName))//'/'//'Analysis'
ModelInfo%InputDir=Trim(AdjustL(ModelInfo%InputDir))//'/'//Trim(AdjustL(ModelInfo%ModelName))//'/'//'Model'

Write (*, fmt="(2A)")" The model directory is: ", ModelInfo%InputDir
Write (*, fmt="(2A)")" The analysis name is: ", ModelInfo%AnalysisDir

! Create the results folder
Write(*,fmt="(A)") " -Creating the output folders ..."

Directory=MakeDirQQ (Trim(AdjustL(ModelInfo%OutputDir))//'/'//Trim(AdjustL(ModelInfo%ModelName)))
  If (Directory) Then
    Write(*,fmt="(A)") "The result folder is created."
  Else
     Write (*,fmt="(A)") "The result folder already exists."
  End If

! Internal folder
Directory=MakeDirQQ (Trim(AdjustL (ModelInfo%IntDir))//'/'//Trim(AdjustL (ModelInfo%ModelName)))
  If (Directory) Then
     Write (*,fmt="(2A)") "The internal folder is created."
  Else
     Write (*,fmt="(2A)") "The internal folder already exists."
  End If

ModelInfo%OutputDir=Trim(AdjustL (ModelInfo%OutputDir))//'/'//Trim(AdjustL (ModelInfo%ModelName))
ModelInfo%IntDir=Trim(AdjustL (ModelInfo%IntDir))//'/'//Trim(AdjustL (ModelInfo%ModelName))

Write (*,fmt="(2A)")" The output directory is: ", ModelInfo%OutputDir

! - Closing the address file ----------------------------------------------------------------------
Write(*,        fmt="(A)") " -Closing the address file"
Write(FileInfo, fmt="(A)") " -Closing the address file"
UnFile =  FileAdr
Close ( Unit = UnFile, Status = 'KEEP', ERR =  1002, IOSTAT = IO_File)


Write(*,       *) 'End Subroutine < Input_Address_sub >'
Write(FileInfo,*) 'End Subroutine < Input_Address_sub >'
Return

! Errors ==========================================================================================
! Opening statement Errors
1001 If (IO_File > 0) Then
       Write(*, Fmt_Err1_OPEN) UnFile, IO_File; Write(FileInfo, Fmt_Err1_OPEN) UnFile, IO_File;
       Write(*, Fmt_FL); Write(FileInfo, Fmt_FL);
       Write(*, Fmt_End); Read(*,*); Stop;
     Else If ( IO_File < 0 ) Then
       Write(*, Fmt_Err1_OPEN) UnFile, IO_File
       Write(FileInfo, Fmt_Err1_OPEN) UnFile, IO_File;  Write(*, Fmt_FL) ; Write(FileInfo, Fmt_FL);
       Write(*, Fmt_End); Read(*,*); Stop;
     End If


! Close statement Errors
1002 If (IO_File > 0) Then
       Write(*, Fmt_Err1_Close) UnFile, IO_File; Write(FileInfo, Fmt_Err1_Close) UnFile, IO_File;
       Write(*, Fmt_FL); Write(FileInfo, Fmt_FL);
       Write(*, Fmt_End); Read(*,*); Stop;
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
!                                                                     & ! integer (1) Variables
!                                                                     & ! integer (2) Variables
!                                                                     & ! integer (4) Variables
!                                                                     & ! integer (8) Variables
!                                                                     & ! real Variables
!                                                                     & ! integer Arrays
!                                                                     & ! real Arrays
!                                                                     & ! Characters
ModelInfo                                                             & ! Type
)

! Libraries =======================================================================================

! User defined modules ============================================================================

Implicit None

! Global Variables ================================================================================

! - integer Variables -----------------------------------------------------------------------------
!#integer (Kind=Shrt), Intent(In)    ::
!#integer (Kind=Shrt), Intent(InOut) ::
!#integer (Kind=Shrt), Intent(OUT)   ::
! - real Variables --------------------------------------------------------------------------------
!#real (Kind=Dbl),     Intent(In)    ::
!#real (Kind=Dbl),     Intent(InOut) ::
!#real (Kind=Dbl),     Intent(OUT)   ::
! - complex Variables -----------------------------------------------------------------------------
!#complex,             Intent(In)    ::
!#complex,             Intent(InOut) ::
!#complex,             Intent(OUT)   ::
! - integer Arrays --------------------------------------------------------------------------------
!#integer (Kind=Shrt), Intent(In),    Dimension (:  )  ::
!#integer (Kind=Shrt), Intent(In),    Dimension (:,:)  ::
!#integer (Kind=Shrt), Intent(In)    ::
!#integer (Kind=Shrt), Intent(InOut) ::
!#integer (Kind=Shrt), Intent(OUT)   ::
! - real Arrays -----------------------------------------------------------------------------------
!#real (Kind=Dbl),     Intent(In),    Dimension (:  )  ::
!#real (Kind=Dbl),     Intent(InOut), Dimension (:  )  ::
!#real (Kind=Dbl),     Intent(OUT),   Dimension (:  )  ::
! - Character Variables ---------------------------------------------------------------------------
!#Character (Kind = ?, Len = ? ) ::
! - Logical Variables -----------------------------------------------------------------------------
!#Logical   ::
! - Types -----------------------------------------------------------------------------------------
type (Input_Data_tp), Intent(In) :: ModelInfo  ! Holds info. (name, dir, output dir) of the model :: ModelInfo  ! Holds info. (name, dir, output dir) of the model

! Local Variables =================================================================================
! - integer Variables -----------------------------------------------------------------------------
integer (Kind=Smll) :: UnFile        ! Holds Unit of a file for error message
integer (Kind=Smll) :: IO_File       ! For IOSTAT: Input Output Status in OPEN command

! - real Variables --------------------------------------------------------------------------------
!#real (Kind=Dbl)      ::
! - complex Variables -----------------------------------------------------------------------------
!#complex              ::
! - integer Arrays --------------------------------------------------------------------------------
!#integer (Kind=Shrt), Dimension (:)  ::
!#integer (Kind=Shrt), Allocatable, Dimension (:)  ::
! - real Arrays -----------------------------------------------------------------------------------
!#real (Kind=Dbl), Dimension (:)      ::
!#real (Kind=Dbl), Allocatable, Dimension (:)  ::
! - Character Variables ---------------------------------------------------------------------------
!#Character (Kind = ?, Len = ? ) ::
! - Logical Variables -----------------------------------------------------------------------------
!#Logical   ::
! - Type ------------------------------------------------------------------------------------------

! code ============================================================================================

write(*,       *) " Subroutine < Input_Basic_sub >: "
write(FileInfo,*) " Subroutine < Input_Basic_sub >: "

! - Opening the data model file -------------------------------------------------------------------
Write(*,        fmt="(A)") " -Opening the input file ..."
Write(FileInfo, fmt="(A)") " -Opening the input file ..."

UnFile=FileDataModel
Open (Unit=UnFile, File=Trim(ModelInfo%ModelName)//'.dataModel', &
      Err=1001, IOStat=IO_File, Access='SEQUENTIAL', ACTION='READ', Asynchronous='NO', &
      Blank='NULL', BLOCKSize=0, DEFAULTFile=Trim(ModelInfo%InputDir), DisPOSE='Keep', Form='Formatted', &
      Position='ASIS', Status='old' )














! - Closing the data model file -------------------------------------------------------------------
Write(*,        fmt="(A)") " -Closing the address file"
Write(FileInfo, fmt="(A)") " -Closing the address file"
UnFile =  FileDataModel
Close ( Unit = UnFile, Status = 'KEEP', ERR =  1002, IOSTAT = IO_File)

Write(*,       *) 'End Subroutine < Input_Basic_sub >'
Write(FileInfo,*) 'End Subroutine < Input_Basic_sub >'
Return

! Errors ==========================================================================================
! Opening statement Errors
1001 If (IO_File > 0) Then
       Write(*, Fmt_Err1_OPEN) UnFile, IO_File; Write(FileInfo, Fmt_Err1_OPEN) UnFile, IO_File;
       Write(*, Fmt_FL); Write(FileInfo, Fmt_FL);
       Write(*, Fmt_End); Read(*,*); Stop;
     Else If ( IO_File < 0 ) Then
       Write(*, Fmt_Err1_OPEN) UnFile, IO_File
       Write(FileInfo, Fmt_Err1_OPEN) UnFile, IO_File;  Write(*, Fmt_FL) ; Write(FileInfo, Fmt_FL);
       Write(*, Fmt_End); Read(*,*); Stop;
     End If


! Close statement Errors
1002 If (IO_File > 0) Then
       Write(*, Fmt_Err1_Close) UnFile, IO_File; Write(FileInfo, Fmt_Err1_Close) UnFile, IO_File;
       Write(*, Fmt_FL); Write(FileInfo, Fmt_FL);
       Write(*, Fmt_End); Read(*,*); Stop;
     End If

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
!                                                                     & ! integer (1) Variables
!                                                                     & ! integer (2) Variables
!                                                                     & ! integer (4) Variables
!                                                                     & ! integer (8) Variables
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
!#integer (Kind=Shrt), Intent(In)    ::
!#integer (Kind=Shrt), Intent(InOut) ::
!#integer (Kind=Shrt), Intent(OUT)   ::
! - real Variables --------------------------------------------------------------------------------
!#real (Kind=Dbl),     Intent(In)    ::
!#real (Kind=Dbl),     Intent(InOut) ::
!#real (Kind=Dbl),     Intent(OUT)   ::
! - complex Variables -----------------------------------------------------------------------------
!#complex,             Intent(In)    ::
!#complex,             Intent(InOut) ::
!#complex,             Intent(OUT)   ::
! - integer Arrays --------------------------------------------------------------------------------
!#integer (Kind=Shrt), Intent(In),    Dimension (:  )  ::
!#integer (Kind=Shrt), Intent(In),    Dimension (:,:)  ::
!#integer (Kind=Shrt), Intent(In)    ::
!#integer (Kind=Shrt), Intent(InOut) ::
!#integer (Kind=Shrt), Intent(OUT)   ::
! - real Arrays -----------------------------------------------------------------------------------
!#real (Kind=Dbl),     Intent(In),    Dimension (:  )  ::
!#real (Kind=Dbl),     Intent(InOut), Dimension (:  )  ::
!#real (Kind=Dbl),     Intent(OUT),   Dimension (:  )  ::
! - Character Variables ---------------------------------------------------------------------------
!#Character (Kind = ?, Len = ? ) ::
! - Logical Variables -----------------------------------------------------------------------------
!#Logical   ::
! - Types -----------------------------------------------------------------------------------------
!#Type() ::

! Local Variables =================================================================================
! - integer Variables -----------------------------------------------------------------------------
!#integer (Kind=Shrt)  ::
! - real Variables --------------------------------------------------------------------------------
!#real (Kind=Dbl)      ::
! - complex Variables -----------------------------------------------------------------------------
!#complex              ::
! - integer Arrays --------------------------------------------------------------------------------
!#integer (Kind=Shrt), Dimension (:)  ::
!#integer (Kind=Shrt), Allocatable, Dimension (:)  ::
! - real Arrays -----------------------------------------------------------------------------------
!#real (Kind=Dbl), Dimension (:)      ::
!#real (Kind=Dbl), Allocatable, Dimension (:)  ::
! - Character Variables ---------------------------------------------------------------------------
!#Character (Kind = ?, Len = ? ) ::
! - Logical Variables -----------------------------------------------------------------------------
!#Logical   ::
! - Type ------------------------------------------------------------------------------------------

! code ============================================================================================

write(*,       *) " Subroutine < Input_Array_sub >: "
write(FileInfo,*) " Subroutine < Input_Array_sub >: "


Write(*,       *) "End Subroutine < Input_Array_sub >"
Write(FileInfo,*) "End Subroutine < Input_Array_sub >"
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
!                                                                     & ! integer (1) Variables
!                                                                     & ! integer (2) Variables
i_analysis,                                                           & ! integer (4) Variables
!                                                                     & ! integer (8) Variables
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
integer (Kind=Smll), Intent(In) :: i_analysis

! - real Variables --------------------------------------------------------------------------------
!#real (Kind=Dbl),     Intent(In)    ::
!#real (Kind=Dbl),     Intent(InOut) ::
!#real (Kind=Dbl),     Intent(OUT)   ::
! - complex Variables -----------------------------------------------------------------------------
!#complex,             Intent(In)    ::
!#complex,             Intent(InOut) ::
!#complex,             Intent(OUT)   ::
! - integer Arrays --------------------------------------------------------------------------------
!#integer (Kind=Shrt), Intent(In),    Dimension (:  )  ::
!#integer (Kind=Shrt), Intent(In),    Dimension (:,:)  ::
!#integer (Kind=Shrt), Intent(In)    ::
!#integer (Kind=Shrt), Intent(InOut) ::
!#integer (Kind=Shrt), Intent(OUT)   ::
! - real Arrays -----------------------------------------------------------------------------------
!#real (Kind=Dbl),     Intent(In),    Dimension (:  )  ::
!#real (Kind=Dbl),     Intent(InOut), Dimension (:  )  ::
!#real (Kind=Dbl),     Intent(OUT),   Dimension (:  )  ::
! - Character Variables ---------------------------------------------------------------------------
!#Character (Kind = ?, Len = ? ) ::
! - Logical Variables -----------------------------------------------------------------------------
!#Logical   ::
! - Types -----------------------------------------------------------------------------------------
type (Input_Data_tp), Intent(inout) :: ModelInfo  ! Holds info. (name, dir, output dir) of the model, Intent(In) :: ModelInfo  ! Holds info. (name, dir, output dir) of the model

! Local Variables =================================================================================
! - integer Variables -----------------------------------------------------------------------------
integer (Kind=Smll) :: UnFile        ! Holds Unit of a file for error message
integer (Kind=Smll) :: IO_File       ! For IOSTAT: Input Output Status in OPEN command

! - real Variables --------------------------------------------------------------------------------
!#real (Kind=Dbl)      ::
! - complex Variables -----------------------------------------------------------------------------
!#complex              ::
! - integer Arrays --------------------------------------------------------------------------------
!#integer (Kind=Shrt), Dimension (:)  ::
!#integer (Kind=Shrt), Allocatable, Dimension (:)  ::
! - real Arrays -----------------------------------------------------------------------------------
!#real (Kind=Dbl), Dimension (:)      ::
!#real (Kind=Dbl), Allocatable, Dimension (:)  ::
! - Character Variables ---------------------------------------------------------------------------
!#Character (Kind = ?, Len = ? ) ::
! - Logical Variables -----------------------------------------------------------------------------
Logical (Kind=Shrt)  :: Directory

! - Type ------------------------------------------------------------------------------------------


! code ============================================================================================
write(*,       *) " Subroutine < Input_Analysis_sub >: "
write(FileInfo,*) " Subroutine < Input_Analysis_sub >: "


! Opening the input file for this specific simulation
Write (*,        fmt="(A)") " -Opening the analysis file ..."
Write (FileInfo, fmt="(A)") " -Opening the analysis file ..."

UnFile=UnInptAna
Open (Unit=UnFile, File=Trim(ModelInfo%AnalysesNames(i_analysis))//'.txt', Err= 1001, IOStat=IO_File, Access='SEQUENTIAL', &
      Action='READ', Asynchronous='NO', Blank='NULL', BLOCKSize=0, DEFAULTFile=Trim(ModelInfo%AnalysisDir), &
      DisPOSE='Keep', FORM='FormATTED', Position='ASIS', Status='old') ;

! Creating the output file directory for this analysis --------------------------------------------
Write(*,        fmt="(A)") " -Creating the output folder for this analysis ..."
Write(FileInfo, fmt="(A)") " -Creating the output folder for this analysis ..."

Directory=MakeDirQQ (Trim(AdjustL (ModelInfo%OutputDir))//'/'//Trim(AdjustL(ModelInfo%AnalysesNames(i_analysis))))
  If (Directory) Then ;
     Write (*,       fmt="(A)") "The output folder for this analysis created." ;
     Write (FileInfo,fmt="(A)") "The output folder for this analysis created." ;
  Else ;
     Write (*,        fmt="(A)") "The output folder for this analysis already exists." ;
     Write (FileInfo, fmt="(A)") "The output folder for this analysis already exists." ;
  End If ;

! Creating the internal File directory ------------------------------------------------------------
Directory=MakeDirQQ (Trim(AdjustL(ModelInfo%IntDir))//'/'//Trim(AdjustL(ModelInfo%AnalysesNames(i_analysis))))
  If (Directory) Then
     Write (*,        fmt="(A)") "The internal folder for this analysis created."
     Write (FileInfo, fmt="(A)") "The internal folder for this analysis created."
  Else
     Write (*,        fmt="(A)") "The internal folder for this analysis already exists."
     Write (FileInfo, fmt="(A)") "The internal folder for this analysis already exists."
  End If

ModelInfo%AnalysisOutputDir=Trim(AdjustL(ModelInfo%OutputDir))//'/'//Trim(AdjustL(ModelInfo%AnalysesNames(i_analysis)))
ModelInfo%AnalysisIntDir=Trim(AdjustL(ModelInfo%IntDir))//'/'//Trim(AdjustL(ModelInfo%AnalysesNames(i_analysis)))





Write(*,       *) "End Subroutine < Input_Analysis_sub >"
Write(FileInfo,*) "End Subroutine < Input_Analysis_sub >"
Return

! Errors ==========================================================================================
! Opening statement Errors
1001 If (IO_File > 0) Then
       Write(*, Fmt_Err1_OPEN) UnFile, IO_File; Write(FileInfo, Fmt_Err1_OPEN) UnFile, IO_File;
       Write(*, Fmt_FL); Write(FileInfo, Fmt_FL);
       Write(*, Fmt_End); Read(*,*); Stop;
     Else If ( IO_File < 0 ) Then
       Write(*, Fmt_Err1_OPEN) UnFile, IO_File
       Write(FileInfo, Fmt_Err1_OPEN) UnFile, IO_File;  Write(*, Fmt_FL) ; Write(FileInfo, Fmt_FL);
       Write(*, Fmt_End); Read(*,*); Stop;
     End If


! Close statement Errors
1002 If (IO_File > 0) Then
       Write(*, Fmt_Err1_Close) UnFile, IO_File; Write(FileInfo, Fmt_Err1_Close) UnFile, IO_File;
       Write(*, Fmt_FL); Write(FileInfo, Fmt_FL);
       Write(*, Fmt_End); Read(*,*); Stop;
     End If

End Subroutine Input_Analysis_sub


end module Input_mod
