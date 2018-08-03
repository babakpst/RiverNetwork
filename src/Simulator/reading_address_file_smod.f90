
submodule (Input_mod) Input_submod

implicit none

contains

!##################################################################################################
!##################################################################################################
module procedure Input_Address_sub

! Libraries =======================================================================================
use ifport

implicit none

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

end procedure Input_Address_sub

!##################################################################################################
!##################################################################################################
module procedure Input_Analysis_sub

! Libraries =======================================================================================
use ifport

! User defined modules ============================================================================
use messages_and_errors_mod

Implicit None

! Local Variables =================================================================================
! - integer Variables -----------------------------------------------------------------------------
integer(kind=Smll) :: UnFile   ! Holds Unit of a file for error message
integer(kind=Smll) :: IO_File  ! For IOSTAT: Input Output status in OPEN command
integer(kind=Smll) :: IO_read  ! Holds error of read statements
integer(kind=Smll) :: IO_write ! Used for IOSTAT - Input Output Status - in the write command.

integer(kind=Lng) :: i_nodes   ! loop index over the total number of nodes in the network
integer(kind=Lng) :: i_reaches ! loop index over the total number of reaches in the network

integer(kind=Lng) :: T_Node    ! temp variable to read the node number
integer(kind=Lng) :: T_Reach   ! temp variable to read the reach number



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
read(unit=UnFile, fmt="(F23.10)", advance='yes', asynchronous='no', iostat=IO_read, &
                  err=1003, end=1004) this%h_dw
UnFile = FileInfo
write(unit=UnFile, fmt="(' Downstream water depth is: ', F23.10, ' m')", advance='yes', &
                   asynchronous='no', iostat=IO_write, err=1006) this%h_dw
write(unit=*,      fmt="(' Downstream water depth is: ', F23.10, ' m')") this%h_dw

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

UnFile = UnInptAna
read(unit=UnFile, fmt="(A)", advance='yes', asynchronous='no', iostat=IO_read, err=1003, end=1004)
read(unit=UnFile, fmt="(A)", advance='yes', asynchronous='no', iostat=IO_read, err=1003, end=1004)
  do i_nodes = 1, this%TotalNNodes
    UnFile = UnInptAna
    read(unit=UnFile, fmt="(I23,F23.10)", advance='yes', asynchronous='no', iostat=IO_read, &
                       err=1003, end=1004) T_Node, this%Q_Up(T_Node)
    UnFile = FileInfo
    write(unit=UnFile, fmt="(' Flow rate at the upstream in the node', I23,' is: ', F23.10, &
    ' m/s3')", advance='yes', asynchronous='no', iostat=IO_write, err=1006)T_Node,this%Q_Up(T_Node)
    write(unit=*     , fmt="(' Flow rate at the upstream in the node', I23,' is: ', F23.10, &
    ' m/s3')", advance='yes', asynchronous='no', iostat=IO_write, err=1006)T_Node,this%Q_Up(T_Node)
  end

UnFile = UnInptAna
read(unit=UnFile, fmt="(A)", advance='yes', asynchronous='no', iostat=IO_read, err=1003, end=1004)
read(unit=UnFile, fmt="(A)", advance='yes', asynchronous='no', iostat=IO_read, err=1003, end=1004)
  do i_reaches = 1, this%TotalNReaches
    UnFile = UnInptAna
    read(unit=UnFile, fmt="(F23.10)", advance='yes', asynchronous='no', iostat=IO_read, err=1003, &
                      end=1004) T_Reach, this%CntrlV(T_Reach)

    UnFile = FileInfo
    write(unit=UnFile,
          fmt="(' The control Volume of the reach no.: ', I23,' is: ', F23.10,' m^3')",&
          advance='yes', asynchronous='no', iostat=IO_write, err=1006) T_Reach,this%CntrlV(T_Reach)

    write(unit=*,
          fmt="(' The control Volume of the reach no.: ', I23,' is: ', F23.10,' m^3')",&
          advance='yes', asynchronous='no', iostat=IO_write, err=1006) T_Reach,this%CntrlV(T_Reach)
  end do

UnFile = UnInptAna
read(unit=UnFile, fmt="(A)", advance='yes', asynchronous='no', iostat=IO_read, err=1003, end=1004)
read(unit=UnFile, fmt="(A)", advance='yes', asynchronous='no', iostat=IO_read, err=1003, end=1004)
  do i_reaches = 1, this%TotalNReaches
    UnFile = UnInptAna
    read(unit=UnFile, fmt="(F23.10)", advance='yes', asynchronous='no', iostat=IO_read, err=1003, &
                      end=1004) T_Reach, this%CntrlV_ratio(T_Reach)

    UnFile = FileInfo
    write(unit=UnFile,
          fmt="(' The control Volume of the reach no.: ', I23,' is: ', F23.10,' m^3')",&
          advance='yes', asynchronous='no', iostat=IO_write, err=1006) &
                                                                 T_Reach,this%CntrlV_ratio(T_Reach)
    write(unit=*,
          fmt="(' The control Volume of the reach no.: ', I23,' is: ', F23.10,' m^3')",&
          advance='yes', asynchronous='no', iostat=IO_write, err=1006) &
                                                                 T_Reach,this%CntrlV_ratio(T_Reach)
  end do

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

end procedure Input_Analysis_sub

!##################################################################################################
!##################################################################################################
module procedure Python_Visualizer_sub

implicit none

! Local variables =================================================================================
! - integer variables -----------------------------------------------------------------------------
integer(kind=Smll) :: UnFile         ! Holds Unit of a file for error message
integer(kind=Smll) :: IO_File        ! For IOSTAT: Input Output Status in OPEN command
integer(kind=Smll) :: IO_write       ! Used for IOSTAT: Input/Output Status in the write command

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

end procedure Python_Visualizer_sub

end submodule Input_submod
