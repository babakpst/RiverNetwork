

!##################################################################################################
! Purpose: This module provides the input geometry files for visualization with Paraview.
!
! Developed by: Babak Poursartip
! Supervised by: Clint Dawson
!
! The Institute for Computational Engineering and Sciences (ICES)
! The University of Texas at Austin
!
! ================================ V E R S I O N ==================================================
! V0.00: 01/24/2019 - Start the module.
! V0.01: 01/28/2019 - compiled successfully for the first time.
!
! File version $Id $
!
! Last update: 01/28/2019
!
! ================================ S U B R O U T I N E ============================================
!
!
! ================================ F U N C T I O N ================================================
!
!
!##################################################################################################


module ParaviewOutput_mod

! Libraries =======================================================================================
use hdf5

! User defined modules ============================================================================
use messages_and_errors_mod
use Parameters_mod

implicit none

private
type ResultReach_tp

  ! contains the solution of this reach, size: no. of cells on this reach
  ! the first element U(1) is height and the second term is U(2) is uh
  type(vector), allocatable, dimension(:) :: U
end type ResultReach_tp

type ResultNetwork_tp

  integer(kind=Lng) :: step     ! holds the time step

  integer(kind=Lng) :: RankNo   ! holds the current Rank no.
  integer(kind=Lng) :: Size     ! holds the size (no. of processors)
  integer(kind=Lng) :: nReach   ! holds no. of reaches on this rank

  ! holds no. of cells on each reach, size: nReach
  integer(kind=Lng), allocatable, dimension(:) :: NoCells

  character (kind = 1, Len = 150) :: OutputDir     ! Directory of output files (Results)

  ! contains all the coordinates of all reaches of the network, size: no. of Reaches on this rank
  type(ResultReach_tp), allocatable, dimension(:)  :: ResultReach

  contains
    !procedure Wrapper   => Wrapper_File_Creator_sub ! wrapper file
    !procedure ReachFile => Reach_File_Creator_sub   ! local xdmf file
    procedure Results   => Result_File_Creator_sub  ! hdf5 creator
end type ResultNetwork_tp

public:: ResultNetwork_tp

contains

!##################################################################################################
! Purpose: This subroutine creates the wrapper file for paraview that contains all reaches in all
!          ranks for all time steps.
!
! Developed by: Babak Poursartip
! Supervised by: Clint Dawson
!
! The Institute for Computational Engineering and Sciences (ICES)
! The University of Texas at Austin
!
! ================================ V E R S I O N ==================================================
! V0.00: 02/11/2019 - File initiated.
! V0.01: 02/11/2019 - Compiled successfully for the first time.
!
! File version $Id $
!
! Last update: 02/11/2019
!
!##################################################################################################

!subroutine Wrapper_File_Creator_sub()

!end subroutine Wrapper_File_Creator_sub

!##################################################################################################
! Purpose: This subroutine creates xdmf file for each reach in each rank for each time step.
!
! Developed by: Babak Poursartip
! Supervised by: Clint Dawson
!
! The Institute for Computational Engineering and Sciences (ICES)
! The University of Texas at Austin
!
! ================================ V E R S I O N ==================================================
! V0.00: 02/11/2019 - File initiated.
! V0.01: 02/11/2019 - Compiled successfully for the first time.
!
! File version $Id $
!
! Last update: 02/11/2019
!
!##################################################################################################

!subroutine Reach_File_Creator_sub()

!end subroutine Reach_File_Creator_sub

!##################################################################################################
! Purpose: This subroutine creates hdf5, result file  (velocity and height) for each reach in each
!          rank for each time step.
!
! Developed by: Babak Poursartip
! Supervised by: Clint Dawson
!
! The Institute for Computational Engineering and Sciences (ICES)
! The University of Texas at Austin
!
! ================================ V E R S I O N ==================================================
! V0.00: 02/11/2019 - File initiated.
! V0.01: 02/11/2019 - Compiled successfully for the first time.
!
! File version $Id $
!
! Last update: 02/11/2019
!
!##################################################################################################

subroutine Result_File_Creator_sub(this)

! Libraries =======================================================================================
use hdf5

! User defined modules ============================================================================

implicit none

! Global variables ================================================================================

! - types -----------------------------------------------------------------------------------------
class(ResultNetwork_tp) :: this

! Local variables =================================================================================
! - integer variables -----------------------------------------------------------------------------
integer(kind=Lng)  :: i_reach       ! loop index on the reach number
integer(kind=Lng)  :: i_cell        ! loop index on the cell number

integer(kind=Smll) :: ERR_Alloc, ERR_DeAlloc ! Allocating and DeAllocating errors

integer(kind=Shrt) :: rank = 2 ! Dataset rank
integer            :: error    ! Error flag

! - real variables --------------------------------------------------------------------------------
real(kind=Dbl), dimension(:,:), allocatable :: dset_data_real ! Data buffers

! - character variables ---------------------------------------------------------------------------
character(kind = 1, len = 3   ), parameter :: res = "Res"
Character(kind = 1, len = 100 ):: IndexReach !Reach no in the Char. fmt to add to input file Name
Character(kind = 1, len = 100 ):: IndexRank  !Rank no in the Char. fmt to add to input file Name
Character(kind = 1, len = 100 ):: IndexSize  !Size of the process in the Char. fmt to add ...
Character(kind = 1, len = 100 ):: IndexStep  !Step no in the Char. fmt to add to input file Name

! - HDF5 variables --------------------------------------------------------------------------------
integer(HID_T) :: id_Results            ! the result h5 file

integer(HID_T) :: dset_id_height        ! data set for height in each cell
integer(HID_T) :: dset_id_velocity      ! data set for velocity in each cell

integer(HID_T) :: dspace_id_height      ! data space for coordinate
integer(HID_T) :: dspace_id_velocity    ! data space for connectivity

integer(HSIZE_T), dimension(2) :: dims  ! data set dimensions

! code ============================================================================================
!write(*,       *) " subroutine < Result_File_Creator_sub >: "
!write(FileInfo,*) " subroutine < Result_File_Creator_sub >: "

! creating the HDF5 files
call h5open_f(error)

! Converting numbers to char for the output file name (results)
write(IndexRank, *) this%RankNo    ! converts rank to Character format for the file Name
write(IndexSize, *) this%Size      ! converts size to Character format for the file Name
write(IndexStep, *) this%Step      ! converts step no. to Character format for the file Name

  do i_reach = 1, this%nReach

    ! Converting numbers to char for the output file name (results)
    write(IndexReach,*) i_reach   ! converts reach no. to Chr format for the file Name

    ! creating the results hdf5 file for each reach in each rank
    call h5fcreate_f(  &
    trim( this%OutputDir)//'/'// &
    trim(res)//'_Ra_'//trim(adjustL(IndexRank))//'_'//trim(adjustL(IndexSize))// &
    '_Re_'//trim(adjustL(IndexReach))//&
    '_St_'//trim(adjustL(IndexStep))//'.h5', &
    H5F_ACC_TRUNC_F, id_Results, error)

    ! working on the height solution --
    dims(1) = 1                      !
    dims(2) = this%NoCells(i_reach)  ! no. of cells from this reach on this rank

    ! creating the data space for the height of water in each cell
    call h5screate_simple_f(rank, dims, dspace_id_height, error)

    ! creating the data set for the height of water in each cell
    call h5dcreate_f(id_Results, "height", H5T_NATIVE_DOUBLE, &
                                                           dspace_id_height, dset_id_height, error)

    ! allocating a real vector to transmit the height solution to the result file
    allocate(dset_data_real( dims(1), dims(2)) , stat=ERR_Alloc)
    if (ERR_Alloc /= 0) call error_in_allocation(ERR_Alloc)

    ! transferring heights from the paraview class to hdf5 file
    dset_data_real(1,:) = this%ResultReach(i_reach)%U(:)%U(1)

    ! Write the dataset- height
    call h5dwrite_f(dset_id_height, H5T_NATIVE_DOUBLE, dset_data_real, dims, error)

    ! working on the velocity solution --
    ! creating the data space for the connectivity of the cells
    call h5screate_simple_f(rank, dims, dspace_id_velocity, error)

    ! creating the data set for the velocity solution
    call h5dcreate_f(id_Results, "velocity", H5T_NATIVE_INTEGER, dspace_id_velocity, &
                                                                          dset_id_velocity, error)

    ! transferring heights from the paraview class to hdf5 file
    forall (i_cell = 1:this%NoCells(i_reach)) dset_data_real(1,i_cell) = &
                this%ResultReach(i_reach)%U(i_cell)%U(2) / this%ResultReach(i_reach)%U(i_cell)%U(1)

    ! Write the dataset- connectivity
    call h5dwrite_f(dset_id_velocity, H5T_NATIVE_DOUBLE, dset_data_real, dims, error)

    DEallocate(dset_data_real, stat = ERR_DeAlloc )
    if (ERR_DeAlloc /= 0) call error_in_deallocation(ERR_DeAlloc)

    ! closing the hdf5 files
    call h5dclose_f(dset_id_height, error)
    call h5dclose_f(dset_id_velocity, error)
    call h5fclose_f(id_Results, error)

  end do

! closing the hdf5 library
call h5close_f(error)

!write(*,       *) " end subroutine < Result_File_Creator_sub >"
!write(FileInfo,*) " end subroutine < Result_File_Creator_sub >"

return

end subroutine Result_File_Creator_sub


end module ParaviewOutput_mod
