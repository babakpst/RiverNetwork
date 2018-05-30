
!##################################################################################################
! Purpose: This module discretizes the domain.
!
! Developed by: Babak Poursartip
! Supervised by: Clint Dawson
!
! The Institute for Computational Engineering and Sciences (ICES)
! The University of Texas at Austin
!
! ================================ V E R S I O N ==================================================
! V0.00: 02/26/2018 - Start the module.
! V0.01: 02/26/2018 - Initiation: Compiled without error for the first time.
! V0.10: 03/08/2018 - Initiated: Compiled without error.
! V1.00: 03/20/2018 - Compiled with no error/warnings.
! V1.10: 04/10/2018 - Minor modifications in the objects/classes.
! V2.10: 05/24/2018 - modifying for MPI
! V2.20: 05/30/2018 - Initializing types
!
! File version $Id $
!
! Last update: 05/30/2018
!
! ================================ S U B R O U T I N E ============================================
! Discretize_1D: Discretizes the 1D model.
! Domain_Func_1D:   Determines the shape of domain
!
! ================================ F U N C T I O N ================================================
!
! ================================ L O C A L   V A R I A B L E S ==================================
! (Refer to the main code to see the list of imported variables)
!  . . . . . . . . . . . . . . . . variables . . . . . . . . . . . . . . . . . . . . . . . . . . .
!
!##################################################################################################

module Discretization_mod

! Libraries =======================================================================================

! User defined modules ============================================================================
use Parameters_mod
use Input_mod, only: Input_Data_tp
use Model_mod, only: Geometry_tp
use Results_mod, only: Plot_domain_1D_tp


implicit none
private

! Contains all information after discretization
type model_tp
  integer (kind=Lng)  :: NCells=0_Lng ! Total number of cells in the domain

  real(kind=DBL), allocatable, dimension(:) :: SlopeCell  ! the slope of each cell at the center
  real(kind=DBL), allocatable, dimension(:) :: SlopeInter ! the slope of each cell at the center
  real(kind=DBL), allocatable, dimension(:) :: ZCell      ! bottom elev. at the center of each cell
  real(kind=DBL), allocatable, dimension(:) :: ZFull      ! bottom elevation at all points
  real(kind=DBL), allocatable, dimension(:) :: ManningCell! the Manning's number of each cell
  real(kind=DBL), allocatable, dimension(:) :: WidthCell  ! the Manning's number of each cell
  real(kind=DBL), allocatable, dimension(:) :: X_Disc     ! the coordinates of the cell center
  real(kind=DBL), allocatable, dimension(:) :: X_Full     ! the coordinates all points

  real(kind=DBL), allocatable, dimension(:,:) :: LengthCell ! the length of each cell
            ! note: the first col holds the actual cell length (length of the controal volume), and
            !       the second col holds the projection(x)

  contains
    procedure Discretize => Discretize_1D_sub

end type model_tp

public:: model_tp

contains

!##################################################################################################
! Purpose: This subroutine discretizes the 1D domain and creates the required arrays.
!
! Developed by: Babak Poursartip
! Supervised by: Clint Dawson
!
! The Institute for Computational Engineering and Sciences (ICES)
! The University of Texas at Austin
!
! ================================ V E R S I O N ==================================================
! V0.00: 02/26/2018 - File initiated.
! V0.01: 00/00/2018 - Initiated: Compiled without error for the first time.
! V1.00: 03/20/2018 - Compiled with no error/warnings.
! V1.10: 04/10/2018 - Minor modifications in the objects/classes.
!
! File version $Id $
!
! Last update: 04/10/2018
!
! ================================ L O C A L   V A R I A B L E S ==================================
! (Refer to the main code to see the list of imported variables)
!  . . . . . . . . . . . . . . . . variables . . . . . . . . . . . . . . . . . . . . . . . . . . .
!
!##################################################################################################

subroutine Discretize_1D_sub(this, Geometry, ModelInfo)

! Libraries =======================================================================================

! User defined modules ============================================================================

implicit none

! Global variables ================================================================================

! - types -----------------------------------------------------------------------------------------
type(Geometry_tp),   intent(in)   :: Geometry   ! Holds the geometry of the domain.
type(Input_Data_tp), intent(in)   :: ModelInfo  ! Holds info. (name, dir, output dir) of the model

class(model_tp), intent(out) :: this ! Discretization

! Local variables =================================================================================
! - integer variables -----------------------------------------------------------------------------
integer(kind=Smll) :: ERR_Alloc, ERR_DeAlloc ! Allocating and DeAllocating errors

integer (kind=Lng) :: i_reach     ! Loop index on the number of reaches
integer (kind=Lng) :: jj          ! Loop index
integer (kind=Lng) :: CellCounter ! Counts number of cells

! - real variables --------------------------------------------------------------------------------
real(kind=Dbl)    :: MaxHeight         ! Maximum height of the domain
real(kind=Dbl)    :: Height            ! Height
real(kind=Dbl)    :: Z_loss            ! loss of height in each cell
real(kind=Dbl)    :: TotalLength       ! Temp var
real(kind=Dbl)    :: CntrlVolumeLength ! The length of control volume
real(kind=Dbl)    :: XCoordinate       ! Temp var to compute the coordinate of the cell center
real(kind=Dbl)    :: ProjectionLength

! - type ------------------------------------------------------------------------------------------
type(Plot_domain_1D_tp(NCells=:)), allocatable :: Plot ! Plots the discretized domain

! code ============================================================================================
write(*,       *) " subroutine < Discretize_1D_sub >: "
write(FileInfo,*) " subroutine < Discretize_1D_sub >: "

write(*,       *) " -Discretizing the domain ..."
write(FileInfo,*) " -Discretizing the domain ..."

! Find total number of cells in the domain:
this%NCells = 0_Lng

write(*,        fmt="(A)") " Calculating the total number of the cells in the domain ... "
write(FileInfo, fmt="(A)") " Calculating the total number of the cells in the domain ... "

  do i_reach = 1_Lng,Geometry%NoReaches
    this%NCells = this%NCells + Geometry%ReachDisc(i_reach)
  end do

write(*,        fmt="(A,I15)") " Total number of cells: ", this%NCells
write(FileInfo, fmt="(A,I15)") " Total number of cells: ", this%NCells

allocate(this%LengthCell(this%NCells,2),            &
         this%SlopeCell(this%NCells),               &
         this%SlopeInter(this%NCells+1),            &
         this%ZCell(this%NCells),                   &
         this%ZFull(this%NCells*2_Lng + 1_Lng),     &
         this%ManningCell(this%NCells),             &
         this%WidthCell(this%NCells),               &
         this%X_Disc(this%NCells),                  &
         this%X_Full(this%NCells*2_Lng + 1_Lng),    &
         stat=ERR_Alloc)

  if (ERR_Alloc /= 0) then
    write (*, Fmt_ALLCT) ERR_Alloc;  write (FileInfo, Fmt_ALLCT) ERR_Alloc;
    write(*, Fmt_FL);  write(FileInfo, Fmt_FL); read(*, Fmt_End);  stop;
  end if

! Finding the highest point in the domain:
write(*,        fmt="(A)")" Calculating the highest point in the domain ... "
write(FileInfo, fmt="(A)")" Calculating the highest point in the domain ... "

MaxHeight = 0.0_Dbl

  do i_reach = 1_Lng,Geometry%NoReaches
    MaxHeight = MaxHeight + Geometry%ReachSlope(i_reach) * Geometry%ReachLength(i_reach)
  end do

write(*,        fmt="(A,F23.10)") " Maximum height is:", MaxHeight
write(FileInfo, fmt="(A,F23.10)") " Maximum height is:", MaxHeight

write(*,        fmt="(A)")" Basic calculations ..."
write(FileInfo, fmt="(A)")" Basic calculations ..."

write(*,        fmt="(A)")" Loop over reaches to discretize the domain ..."
write(FileInfo, fmt="(A)")" Loop over reaches to discretize the domain ..."

CellCounter = 0_Lng

  do i_reach = 1_Lng, Geometry%NoReaches  ! Loop over the reaches

    write(*,        fmt="(A, I10,A)")" -Discretizing reach no.: ", i_reach, " ..."
    write(FileInfo, fmt="(A, I10,A)")" -Discretizing reach no.: ", i_reach, " ..."

      if (Geometry%ReachType(i_reach)==0_Shrt) then

        CntrlVolumeLength = floor(Geometry%ReachLength(i_reach)*1.0E10/ &
                            Geometry%ReachDisc(i_reach), kind=Lng)/1.0E10 ! Control volume length
        write(*,fmt="(A,I5,A,F23.10)")" Cell length in the reach ",i_reach," is:",CntrlVolumeLength

        Height = MaxHeight
        Z_loss = floor(1.0E10*CntrlVolumeLength * Geometry%ReachSlope(i_reach))/ 1.0E10
        Height = Height +  floor(1.0E10 * 0.5_Dbl * Z_loss)/1.0E10
        TotalLength = 0.0_Dbl
        XCoordinate = 0.5_Dbl * CntrlVolumeLength

          do jj = 1_Lng,i_reach-1_Lng
            XCoordinate = XCoordinate + Geometry%ReachLength(jj)
          end do

          do jj = 1_Lng, Geometry%ReachDisc(i_reach) - 1_Lng
              CellCounter = CellCounter + 1_Lng
              this%LengthCell(CellCounter,1)= CntrlVolumeLength
              this%LengthCell(CellCounter,2)= CntrlVolumeLength ! <modify> use horizontal distance
              this%X_Disc(CellCounter)      = XCoordinate
              this%X_Full(CellCounter)      = XCoordinate - 0.5_Dbl * CntrlVolumeLength
              this%X_Full(CellCounter*2_Lng+1_Lng) = XCoordinate

              XCoordinate = XCoordinate + CntrlVolumeLength
              TotalLength = TotalLength + CntrlVolumeLength
              Height      = Height - Z_loss

              this%SlopeCell(CellCounter)  = Geometry%ReachSlope(i_reach)
              this%SlopeInter(CellCounter) = Geometry%ReachSlope(i_reach)
              this%ZCell(CellCounter)      = Height
              this%ZFull(CellCounter*2)    = Height + 0.5_Dbl * Z_loss
              this%ZFull(CellCounter*2+1)  = Height
              this%ManningCell(CellCounter)= Geometry%ReachManning(i_reach)
              this%WidthCell(CellCounter)  = Geometry%ReachWidth(i_reach)
          end do

        CellCounter = CellCounter + 1_Lng
        this%LengthCell(CellCounter,1) = Geometry%ReachLength(i_reach) - TotalLength

        this%SlopeCell(CellCounter)  = Geometry%ReachSlope(i_reach)
        this%SlopeInter(CellCounter) = Geometry%ReachSlope(i_reach)

        XCoordinate = XCoordinate-0.5_dbl*CntrlVolumeLength+0.5_dbl*this%LengthCell(CellCounter,1)

        this%X_Disc(CellCounter)     = XCoordinate
        this%X_Full(CellCounter*2_Lng)       = XCoordinate - 0.5 * this%LengthCell(CellCounter,1)
        this%X_Full(CellCounter*2_Lng+1_Lng) = XCoordinate
        this%X_Full(CellCounter*2_Lng+2_Lng) = XCoordinate + 0.5 * this%LengthCell(CellCounter,1)

        Height = Height - (0.5_dbl * Z_loss + 0.5_dbl * this%LengthCell(CellCounter,1) &
                                                      * Geometry%ReachSlope(i_reach))

        this%ZCell(CellCounter)             = Height

        this%ZFull(CellCounter*2_Lng)       = Height + 0.5_dbl * this%LengthCell(CellCounter,1) &
                                                               * Geometry%ReachSlope(i_reach)
        this%ZFull(CellCounter*2_Lng+1_Lng) = Height
        this%ZFull(CellCounter*2_Lng)       = Height - 0.5_dbl * this%LengthCell(CellCounter,1) &
                                                               * Geometry%ReachSlope(i_reach)
        this%ManningCell(CellCounter)= Geometry%ReachManning(i_reach)
        this%WidthCell(CellCounter)  = Geometry%ReachWidth(i_reach)

        MaxHeight = MaxHeight - Geometry%ReachLength(i_reach) * Geometry%ReachSlope(i_reach)

      else if (Geometry%ReachType(i_reach)==1_Shrt) then

        Height = MaxHeight
        ProjectionLength = floor(1.0E10 * Geometry%ReachLength(i_reach)/&
                           Geometry%ReachDisc(i_reach) )/1.0E10
        XCoordinate = 0.5_Dbl * ProjectionLength

          do jj = 1_Lng,i_reach-1_Lng
            XCoordinate = XCoordinate + Geometry%ReachLength(jj)
          end do

          do jj = 1_Lng, Geometry%ReachDisc(i_reach)
            CellCounter = CellCounter + 1_Lng
            Z_loss = Domain_Func_1D(XCoordinate)

            this%LengthCell(CellCounter,1) = dsqrt(ProjectionLength**2 + Z_loss**2)
            this%LengthCell(CellCounter,2) = ProjectionLength
            this%X_Disc(CellCounter) = XCoordinate
            this%X_Full(CellCounter*2_Lng)       = XCoordinate - 0.5_Dbl * ProjectionLength
            this%X_Full(CellCounter*2_Lng+1_Lng) = XCoordinate

            this%SlopeCell(CellCounter)  = Domain_Func_1D_D(XCoordinate)
            this%SlopeInter(CellCounter) = Domain_Func_1D_D(XCoordinate-0.5_Dbl * ProjectionLength)
            this%ZCell(CellCounter)      = Height + Z_loss
            this%ZFull(CellCounter*2)    = Height &
                                         + Domain_Func_1D(XCoordinate - 0.5_Dbl * ProjectionLength)
            this%ZFull(CellCounter*2+1)  = Height + Z_loss
            this%ManningCell(CellCounter)= Geometry%ReachManning(i_reach)
            this%WidthCell(CellCounter)  = Geometry%ReachWidth(i_reach)

            XCoordinate = XCoordinate + ProjectionLength
            TotalLength = TotalLength + ProjectionLength
          end do

        MaxHeight = MaxHeight - Geometry%ReachLength(i_reach) * Geometry%ReachSlope(i_reach)
      end if

  end do

  if (this%NCells /= CellCounter) then
    write(*,        fmt = "(A,2I10)") "Fatal error: Mismatch between the number of cells. &
                          Check the discretization module.", this%NCells, CellCounter
    write(FileInfo, fmt = "(A,2I10)") "Fatal error: Mismatch between the number of cells. &
                          Check the discretization module.", this%NCells, CellCounter
    write(*, Fmt_End); read(*,*); stop;
  end if

write(*,        fmt="(' Discretization was successful. ')")
write(FileInfo, fmt="(' Discretization was successful. ')")

write(*,        fmt="(' -Plotting the discretized domain ... ')")
write(FileInfo, fmt="(' -Plotting the discretized domain ... ')")

! Plot the discretized domain (cell centers)
allocate(Plot_domain_1D_tp(CellCounter) :: Plot, stat=ERR_Alloc)

  if (ERR_Alloc /= 0) then
    write (*, Fmt_ALLCT) ERR_Alloc;  write (FileInfo, Fmt_ALLCT) ERR_Alloc;
    write(*, Fmt_FL);  write(FileInfo, Fmt_FL); read(*, Fmt_End);  stop;
  end if

! Filling the coordinates for plot
Plot%XCoor(:)      = this%X_Disc(:)
Plot%ZCoor(:)      = this%ZCell(:)
Plot%SlopeCell(:)  = this%SlopeCell(:)
Plot%IndexSize     = Geometry%IndexSize

call Plot%plot(ModelInfo)

write(*,       *) " end subroutine < Discretize_1D_sub >"
write(FileInfo,*) " end subroutine < Discretize_1D_sub >"

return
end subroutine Discretize_1D_sub

!##################################################################################################
! Purpose: This function determines the shape of the 1D domain.
!
! Developed by: Babak Poursartip
! Supervised by: Clint Dawson
!
! The Institute for Computational Engineering and Sciences (ICES)
! The University of Texas at Austin
!
! ================================ V E R S I O N ==================================================
! V0.00: 02/26/2018 - File initiated.
! V0.01: 02/26/2018 - Initiated: Compiled without error for the first time.
!
! File version $Id $
!
! Last update: 02/26/2018
!
! ================================ L O C A L   V A R I A B L E S ==================================
! (Refer to the main code to see the list of imported variables)
!  . . . . . . . . . . . . . . . . variables . . . . . . . . . . . . . . . . . . . . . . . . . . .
! x: The location of the domain
! Bathymetry: The height of the domain
! DBathymetry: The derivative of the domain at one specific point.
!
!##################################################################################################

function Domain_Func_1D(x) result(Bathymetry)

implicit none

real(kind=Dbl)  :: x
real(kind=Dbl)  :: Bathymetry


! code ============================================================================================

Bathymetry = 0.2_Dbl - 0.05_Dbl * (x-10.0_Dbl)**2

end function Domain_Func_1D

!##################################################################################################
function Domain_Func_1D_D(x) result(DBathymetry)

implicit none

real(kind=Dbl) :: x
real(kind=Dbl) :: DBathymetry

! code ============================================================================================
DBathymetry = - 0.05_Dbl * 2.0_Dbl * (x-10.0_Dbl)

end function Domain_Func_1D_D

end module Discretization_mod
