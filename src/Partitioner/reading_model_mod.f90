
!##################################################################################################
! Purpose: This module reads all data for simulation.
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
! V1.00: 04/10/2018 - Major modifications
! V2.00: 04/17/2018 - Partitioner
! V2.10: 05/15/2018 - Separating the input model from the input address
! V2.20: 05/30/2018 - Initializing types
! V3.00: 06/07/2018 - Reading network
!
! File version $Id $
!
! Last update: 06/07/2018
!
! ================================ S U B R O U T I N E ============================================
! reading_initial_info_on_network_sub
! reading_network_geometry_sub
! ================================ F U N C T I O N ================================================
!
! ================================ L O C A L   V A R I A B L E S ==================================
! (Refer to the main code to see the list of imported variables)
!  . . . . . . . . . . . . . . . . Variables . . . . . . . . . . . . . . . . . . . . . . . . . . .
!
!##################################################################################################

module Model_mod

use Parameters_mod
use Input_mod, only: Input_Data_tp
use messages_and_errors_mod

implicit none
private


! Contains the information we read from the base input file.
type Base_Geometry_tp
  integer(kind=Lng) :: NoReaches=0_Lng ! Number of reaches
  integer(kind=Lng) :: NoNodes=0_Lng   ! Number of nodes
  integer(kind=Shrt):: size=0          ! Number of partitions/ranks

  Character(kind = 1,len = 20):: IndexSize="   "!Size no in the Char. fmt to add to input file Name

  contains
    procedure initial_network_info => reading_initial_info_on_network_sub
end type Geometry_tp

! This type contains all required information for each reach.
type reach_tp
  integer(kind=Lng)  :: ReachDisc=0_Lng  ! no. of control volumes (cells) in each reach
  integer(kind=Shrt) :: ReachType=0_Shrt ! reach type - 0 for straight, 1 for geometry form func.
  integer(kind=Lng), dimension(2) :: ReachNodes=0_Lng ! holds the node number at the each
                                                      ! reach, start and end node number
  real(kind=DBL) :: ReachLength=0.0_dbl  ! the length of each reach
  real(kind=DBL) :: ReachSlope=0.0_dbl   ! the slope of each reach
  real(kind=DBL) :: ReachManning=0.0_dbl ! the Manning's number for each reach
  real(kind=DBL) :: ReachWidth=0.0_dbl   ! Stores the width of each reach

  real(kind=DBL), dimension(2) :: ReachAngle=0.0_dbl ! Stores the orientation of the reach
                                                     ! in comparison to the vertical axis

  real(kind=DBL), dimension(2) :: JunctionLength=0.0_dbl ! Length of the junction
end type reach_tp


! Contains all information about the geometry of the domain. (input)
type Geometry_tp

  type(reach_tp), allocatable, dimension(:):: network
  type(Base_Geometry_tp) :: Base_Geometry

  contains
    procedure reading_network => reading_network_geometry_sub
end type Geometry_tp

public:: Geometry_tp

contains

!##################################################################################################
! Purpose: This subroutine reads the initial information for simulation, so that the arrays can be
!          allocated.
!
! Developed by:  Babak Poursartip
! Supervised by: Clint Dawson
!
! The Institute for Computational Engineering and Sciences (ICES)
! The University of Texas at Austin
!
! ================================ V E R S I O N ==================================================
! V0.10: 02/21/2018 - Initiation.
! V1.00: 03/01/2018 - Compiled without error.
! V1.10: 04/10/2018 - Minor modifications.
! V2.00: 06/07/2018 - Modifications for network.
!
! File version $Id $
!
! Last update: 06/07/2018
!
! ================================ L O C A L   V A R I A B L E S ==================================
! (Refer to the main code to see the list of imported variables)
!  . . . . . . . . . . . . . . . . Variables . . . . . . . . . . . . . . . . . . . . . . . . . . .
!
!##################################################################################################

Subroutine reading_initial_info_on_network_sub(this, ModelInfo)

! Libraries =======================================================================================

! User defined modules ============================================================================

Implicit None

! Global Variables ================================================================================

! - Types -----------------------------------------------------------------------------------------
type(Input_Data_tp),  intent(In)  :: ModelInfo ! Holds info. (name, dir, output dir) of the model
class(Base_Geometry_tp), intent(out) :: this  ! Holds information about the geometry of the domain

! Local Variables =================================================================================
! - integer Variables -----------------------------------------------------------------------------
integer(kind=Smll) :: UnFile   ! Holds Unit of a file for error message
integer(kind=Smll) :: IO_File  ! For IOSTAT: Input Output status in OPEN command
integer(kind=Smll) :: IO_read  ! Holds error of read statements
integer(kind=Smll) :: IO_write ! Used for IOSTAT - Input Output Status - in the write command.

! code ============================================================================================
write(*,       *)
write(*,       *) " Subroutine < reading_initial_info_on_network_sub >: "
write(FileInfo,*)
write(FileInfo,*) " Subroutine < reading_initial_info_on_network_sub >: "

! - Opening the data model file -------------------------------------------------------------------
write(*,        fmt="(A)") " -Opening the data model file ..."
write(FileInfo, fmt="(A)") " -Opening the data model file ..."

UnFile=FileDataModel
open(Unit=UnFile, file=trim(ModelInfo%ModelName)//'.dataModel', &
     err=1001, iostat=IO_File, access='sequential', action='read', asynchronous='no', &
     blank='null', blocksize=0, defaultfile=trim(ModelInfo%InputDir), DisPOSE='keep', &
     form='formatted', position='asis', status='old')

UnFile = FileDataModel  ! Total number of reaches in the domain
read(unit=UnFile, fmt="(A)", advance='yes', asynchronous='no', iostat=IO_read, err=1003, end=1004)
read(unit=UnFile, fmt="(A)", advance='yes', asynchronous='no', iostat=IO_read, err=1003, end=1004)
read(unit=UnFile, fmt="(A)", advance='yes', asynchronous='no', iostat=IO_read, err=1003, end=1004)
read(unit=UnFile,fmt="(I10)",advance='yes', asynchronous='no', iostat=IO_read, err=1003, end=1004)&
                                                                                     this%NoReaches

UnFile = FileInfo
write(unit=UnFile, fmt="(' Total number of reach(es) is(are): ', I10)", advance='yes', &
                   asynchronous='no', iostat=IO_write, err=1006) this%NoReaches
write(unit=*,      fmt="(' Total number of reach(es) is(are): ', I10)") this%NoReaches

UnFile = FileDataModel  ! Number of cores - required for mesh partitioning.
read(unit=UnFile, fmt="(A)", advance='yes', asynchronous='no', iostat=IO_read, err=1003, end=1004)
read(unit=UnFile, fmt="(A)", advance='yes', asynchronous='no', iostat=IO_read, err=1003, end=1004)
read(unit=UnFile,fmt="(I10)",advance='yes', asynchronous='no', iostat=IO_read, err=1003, end=1004)&
                                                                                          this%size

UnFile = FileInfo
write(unit=UnFile, fmt="(' Total number of core(s) is(are): ', I10)", advance='yes', &
      asynchronous='no', iostat=IO_write, err=1006) this%size
write(unit=*,      fmt="(' Total number of core(s) is(are): ', I10)") this%size


write(this%IndexSize, *) this%size ! Converts Size to Character format for the file Name



! - Closing the data model file -------------------------------------------------------------------
write(*,        fmt="(A)") " -Closing the data model file"
write(FileInfo, fmt="(A)") " -Closing the data model file"

UnFile =  FileDataModel
Close(Unit = UnFile, status = 'keep', ERR =  1002, IOSTAT = IO_File)

write(*,       *) ' End Subroutine < reading_initial_info_on_network_sub >'
write(*,       *)
write(FileInfo,*) ' End Subroutine < reading_initial_info_on_network_sub >'
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


End Subroutine reading_initial_info_on_network_sub


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

Subroutine reading_network_geometry_sub(this, ModelInfo)


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

integer(kind=Lng)  :: i_reach  ! loop index on the number of reaches
integer(kind=Lng)  :: reach_no ! loop index on the number of reaches

! code ============================================================================================
write(*,       *)
write(*,       *) " Subroutine < reading_network_geometry_sub >: "
write(FileInfo,*)
write(FileInfo,*) " Subroutine < reading_network_geometry_sub >: "

! Open required Files -----------------------------------------------------------------------------
write(*,        fmt="(A)") " -Opening the input files for arrays ..."
write(FileInfo, fmt="(A)") " -Opening the input files for arrays ..."

! Open the input file for arrays
UnFile = FileDataGeo
open(Unit=UnFile, file=trim(ModelInfo%ModelName)//'.Geo', &
     err=1001, iostat=IO_File, access='sequential', action='read', asynchronous='no', &
     blank='null', blocksize=0, defaultfile=trim(ModelInfo%InputDir), DisPOSE='keep', &
     form='formatted', position='asis', status='old')

! reading the network
UnFile = FileDataGeo
read(unit=UnFile, fmt="(A)", advance='yes', asynchronous='no', iostat=IO_read, err=1003, end=1004)
read(unit=UnFile, fmt="(A)", advance='yes', asynchronous='no', iostat=IO_read, err=1003, end=1004)
read(unit=UnFile, fmt="(A)", advance='yes', asynchronous='no', iostat=IO_read, err=1003, end=1004)
read(unit=UnFile, fmt="(A)", advance='yes', asynchronous='no', iostat=IO_read, err=1003, end=1004)


! Reading the network info
  do i_reach= 1, this%NoReaches
    UnFile = FileDataGeo
    read(unit=UnFile, fmt="(F23.10)", advance='yes', asynchronous='no', iostat=IO_read,  &
         err=1003, end=1004) &
         reach_no,  & ! reach no.
         this%ReachLength(reach_no), & ! Length of the reach



  end do


UnFile = FileInfo
write(unit=*,      fmt="(' The length of reach ', I5, ' is:', F23.10,' m')") &
                                                            i_reach, this%ReachLength(i_reach)
write(unit=UnFile, fmt="(' The length of reach ', I5, ' is:', F23.10,' m')") &
                                                            i_reach, this%ReachLength(i_reach)




! Reading total number of control volumes in each reach/For now we have a constant discretization
UnFile = FileDataGeo
read(unit=UnFile, fmt="(A)", advance='yes', asynchronous='no', iostat=IO_read, err=1003, end=1004)
read(unit=UnFile, fmt="(A)", advance='yes', asynchronous='no', iostat=IO_read, err=1003, end=1004)
  do i_reach= 1, this%NoReaches
    UnFile = FileDataGeo
    read(unit=UnFile, fmt="(I10)", advance='yes', asynchronous='no', iostat=IO_read, err=1003, &
         end=1004) this%ReachDisc(i_reach)
    UnFile = FileInfo
    write(unit=*,      fmt="(' No. of discretization of reach ', I5, ' is:', I10)") &
                                                                  i_reach, this%ReachDisc(i_reach)
    write(unit=UnFile, fmt="(' No. of discretization of reach ', I5, ' is:', I10)") &
                                                                  i_reach, this%ReachDisc(i_reach)
  end do

! Reading reach type
UnFile = FileDataGeo
read(unit=UnFile, fmt="(A)", advance='yes', asynchronous='no', iostat=IO_read, err=1003, end=1004)
read(unit=UnFile, fmt="(A)", advance='yes', asynchronous='no', iostat=IO_read, err=1003, end=1004)
  do i_reach= 1, this%NoReaches
    UnFile = FileDataGeo
    read(unit=UnFile, fmt="(I10)", advance='yes', asynchronous='no', iostat=IO_read, err=1003, &
         end=1004) this%ReachType(i_reach)

    UnFile = FileInfo
    write(unit=*,     fmt="(' Rreach ', I10,' is of type: ', I5)") i_reach, this%ReachType(i_reach)
    write(unit=UnFile,fmt="(' Rreach ', I10,' is of type: ', I5)") i_reach, this%ReachType(i_reach)
  end do

! Reading slopes of reach
UnFile = FileDataGeo
read(unit=UnFile, fmt="(A)", advance='yes', asynchronous='no', iostat=IO_read, err=1003, end=1004)
read(unit=UnFile, fmt="(A)", advance='yes', asynchronous='no', iostat=IO_read, err=1003, end=1004)
  do i_reach= 1, this%NoReaches
    UnFile = FileDataGeo
    read(unit=UnFile, fmt="(F23.10)", advance='yes', asynchronous='no', iostat=IO_read, err=1003, &
         end=1004) this%ReachSlope(i_reach)

    UnFile = FileInfo
    write(unit=*,      fmt="(' The slope of reach ', I10,' is: ', F23.10)") &
                                                                  i_reach, this%ReachSlope(i_reach)
    write(unit=UnFile, fmt="(' The slope of reach ', I10,' is: ', F23.10)") &
                                                                  i_reach, this%ReachSlope(i_reach)
  end do

! Reading Manning number of each reach
UnFile = FileDataGeo
read(unit=UnFile, fmt="(A)", advance='yes', asynchronous='no', iostat=IO_read, err=1003, end=1004)
read(unit=UnFile, fmt="(A)", advance='yes', asynchronous='no', iostat=IO_read, err=1003, end=1004)
  do i_reach= 1, this%NoReaches
    UnFile = FileDataGeo
    read(unit=UnFile, fmt="(F23.10)", advance='yes', asynchronous='no', iostat=IO_read, err=1003, &
         end=1004) this%ReachManning(i_reach)

    UnFile = FileInfo
    write(unit=*,      fmt="(' The Mannings no. for reach ', I10,' is: ', F23.10)") &
                                                                i_reach, this%ReachManning(i_reach)
    write(unit=UnFile, fmt="(' The Mannings no. for reach ', I10,' is: ', F23.10)") &
                                                                i_reach, this%ReachManning(i_reach)
  end do

! Reading the width of each reach
UnFile = FileDataGeo
read(unit=UnFile, fmt="(A)", advance='yes', asynchronous='no', iostat=IO_read, err=1003, end=1004)
read(unit=UnFile, fmt="(A)", advance='yes', asynchronous='no', iostat=IO_read, err=1003, end=1004)
  do i_reach= 1, this%NoReaches

    UnFile = FileDataGeo
    read(unit=UnFile, fmt="(F23.10)", advance='yes', asynchronous='no', iostat=IO_read, err=1003, &
         end=1004) this%ReachWidth(i_reach)

    UnFile = FileInfo
    write(unit=*,      fmt="(' The width of reach ', I10,' is: ', F23.10)") &
                                                                  i_reach, this%ReachWidth(i_reach)
    write(unit=UnFile, fmt="(' The width of reach ', I10,' is: ', F23.10)") &
                                                                  i_reach, this%ReachWidth(i_reach)
  end do



















! - Closing the geometry file ---------------------------------------------------------------------
write(*,        fmt="(A)") " -Closing the geometry file"
write(FileInfo, fmt="(A)") " -Closing the geometry file"

UnFile = FileDataGeo
Close(Unit = UnFile, status = 'keep', ERR =  1002, IOSTAT = IO_File)

write(*,       *) " End Subroutine < reading_network_geometry_sub >"
write(*,       *)
write(FileInfo,*) " End Subroutine < reading_network_geometry_sub >"
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

end Subroutine reading_network_geometry_sub

end module Model_mod

