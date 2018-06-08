
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
! V3.20: 06/08/2018 - network discretization
!
! File version $Id $
!
! Last update: 06/08/2018
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
use messages_and_errors_mod

implicit none
private

! contains all information after discretization
type DiscretizedReach_tp
  integer (kind=Lng)  :: NCells_reach=0_Lng ! number of cells in the reach

  real(kind=DBL), allocatable, dimension(:) :: CellSlope      ! slope of each cell at the center
  real(kind=DBL), allocatable, dimension(:) :: InterfaceSlope ! slope of each cell at the center
  real(kind=DBL), allocatable, dimension(:) :: ZCell     ! bottom elev. at the center of each cell
  real(kind=DBL), allocatable, dimension(:) :: XCell     ! the coordinates of the cell center

  real(kind=DBL), allocatable, dimension(:) :: ZFull     ! bottom elevation at cells and interfaces
  real(kind=DBL), allocatable, dimension(:) :: XFull     ! coordinates at cells and interfaces

  real(kind=DBL), allocatable, dimension(:) :: LengthCell ! the length of each cell

  real(kind=DBL) :: ReachManning          ! the Manning's number of each cell
  real(kind=DBL) :: ReachWidthCell        ! the Manning's number of each cell
  real(kind=DBL) :: CellPorjectionLength  ! the length of each cell in the horizontal dir.
end type DiscretizedReach_tp


! contains the information about the entire network, separated by reaches
type DiscretizedNetwork_tp

  integer (kind=Lng)  :: NCells=0_Lng  ! Total number of cells in the network
  type(DiscretizedReach_tp), allocatable, dimension(:) :: DiscretizedReach ! info about each reach

  contains
    procedure Discretize => Discretize_1D_sub ! subroutine to discretize the

end type DiscretizedNetwork_tp

public:: DiscretizedNetwork_tp

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
! V2.00: 06/08/2018 - Network discretization
!
! File version $Id $
!
! Last update: 06/08/2018
!
! ================================ L O C A L   V A R I A B L E S ==================================
! (Refer to the main code to see the list of imported variables)
!  . . . . . . . . . . . . . . . . variables . . . . . . . . . . . . . . . . . . . . . . . . . . .
!
!##################################################################################################

subroutine Discretize_network_sub(this, Geometry, ModelInfo)

! Libraries =======================================================================================

! User defined modules ============================================================================

implicit none

! Global variables ================================================================================

! - types -----------------------------------------------------------------------------------------
type(Geometry_tp),   intent(in)   :: Geometry   ! Holds the geometry of the domain.
type(Input_Data_tp), intent(in)   :: ModelInfo  ! Holds info. (name, dir, output dir) of the model

class(DiscretizedNetwork_tp), intent(out) :: this ! Discretization

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
    this%NCells = this%NCells + Geometry%ReachCells(i_reach)
  end do

write(*,        fmt="(A,I15)") " Total number of cells: ", this%NCells
write(FileInfo, fmt="(A,I15)") " Total number of cells: ", this%NCells

allocate(this%LengthCell(this%NCells,2),            &
         this%CellSlope(this%NCells),               &
         this%InterfaceSlope(this%NCells+1),            &
         this%ZCell(this%NCells),                   &
         this%ZFull(this%NCells*2_Lng + 1_Lng),     &
         this%ManningCell(this%NCells),             &
         this%WidthCell(this%NCells),               &
         this%XCell(this%NCells),                  &
         this%XFull(this%NCells*2_Lng + 1_Lng),    &
         stat=ERR_Alloc)
  if (ERR_Alloc /= 0) call error_in_allocation(ERR_Alloc)

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
                            Geometry%ReachCells(i_reach), kind=Lng)/1.0E10 ! Control volume length
        write(*,fmt="(A,I5,A,F23.10)")" Cell length in the reach ",i_reach," is:",CntrlVolumeLength

        Height = MaxHeight
        Z_loss = floor(1.0E10*CntrlVolumeLength * Geometry%ReachSlope(i_reach))/ 1.0E10
        Height = Height +  floor(1.0E10 * 0.5_Dbl * Z_loss)/1.0E10
        TotalLength = 0.0_Dbl
        XCoordinate = 0.5_Dbl * CntrlVolumeLength

          do jj = 1_Lng,i_reach-1_Lng
            XCoordinate = XCoordinate + Geometry%ReachLength(jj)
          end do

          do jj = 1_Lng, Geometry%ReachCells(i_reach) - 1_Lng
              CellCounter = CellCounter + 1_Lng
              this%LengthCell(CellCounter,1)= CntrlVolumeLength
              this%LengthCell(CellCounter,2)= CntrlVolumeLength ! <modify> use horizontal distance
              this%XCell(CellCounter)      = XCoordinate
              this%XFull(CellCounter)      = XCoordinate - 0.5_Dbl * CntrlVolumeLength
              this%XFull(CellCounter*2_Lng+1_Lng) = XCoordinate

              XCoordinate = XCoordinate + CntrlVolumeLength
              TotalLength = TotalLength + CntrlVolumeLength
              Height      = Height - Z_loss

              this%CellSlope(CellCounter)  = Geometry%ReachSlope(i_reach)
              this%InterfaceSlope(CellCounter) = Geometry%ReachSlope(i_reach)
              this%ZCell(CellCounter)      = Height
              this%ZFull(CellCounter*2)    = Height + 0.5_Dbl * Z_loss
              this%ZFull(CellCounter*2+1)  = Height
              this%ManningCell(CellCounter)= Geometry%ReachManning(i_reach)
              this%WidthCell(CellCounter)  = Geometry%ReachWidth(i_reach)
          end do

        CellCounter = CellCounter + 1_Lng
        this%LengthCell(CellCounter,1) = Geometry%ReachLength(i_reach) - TotalLength

        this%CellSlope(CellCounter)  = Geometry%ReachSlope(i_reach)
        this%InterfaceSlope(CellCounter) = Geometry%ReachSlope(i_reach)

        XCoordinate = XCoordinate-0.5_dbl*CntrlVolumeLength+0.5_dbl*this%LengthCell(CellCounter,1)

        this%XCell(CellCounter)     = XCoordinate
        this%XFull(CellCounter*2_Lng)       = XCoordinate - 0.5 * this%LengthCell(CellCounter,1)
        this%XFull(CellCounter*2_Lng+1_Lng) = XCoordinate
        this%XFull(CellCounter*2_Lng+2_Lng) = XCoordinate + 0.5 * this%LengthCell(CellCounter,1)

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
        ProjectionLength = floor(1.0E9 * Geometry%ReachLength(i_reach)/&
                           Geometry%ReachCells(i_reach) )/1.0E9
        XCoordinate = 0.5_Dbl * ProjectionLength

          do jj = 1_Lng,i_reach-1_Lng
            XCoordinate = XCoordinate + Geometry%ReachLength(jj)
          end do

          do jj = 1_Lng, Geometry%ReachCells(i_reach)
            CellCounter = CellCounter + 1_Lng
            !Z_loss = Domain_Func_1D(XCoordinate)
            Z_loss = Domain_Func_1D_MacDonald(XCoordinate)

            this%LengthCell(CellCounter,1) = dsqrt(ProjectionLength**2 + Z_loss**2)
            this%LengthCell(CellCounter,2) = ProjectionLength
            this%XCell(CellCounter) = XCoordinate
            this%XFull(CellCounter*2_Lng)       = XCoordinate - 0.5_Dbl * ProjectionLength
            this%XFull(CellCounter*2_Lng+1_Lng) = XCoordinate


            !this%CellSlope(CellCounter)  = Domain_Func_1D_D(XCoordinate)
            !this%InterfaceSlope(CellCounter) = Domain_Func_1D_D(XCoordinate-0.5_Dbl * ProjectionLength)
            this%CellSlope(CellCounter)  = Domain_Func_1D_MacDonald_D(XCoordinate)
            this%InterfaceSlope(CellCounter) = Domain_Func_1D_MacDonald_D(XCoordinate-0.5_Dbl * ProjectionLength)
            this%ZCell(CellCounter)      = Height + Z_loss
            !this%ZFull(CellCounter*2)    = Height &
            !                             + Domain_Func_1D(XCoordinate - 0.5_Dbl * ProjectionLength)

            this%ZFull(CellCounter*2)    = Height &
                                         + Domain_Func_1D_MacDonald(XCoordinate - 0.5_Dbl * ProjectionLength)

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
if (ERR_Alloc /= 0) call error_in_allocation(ERR_Alloc)

! Filling the coordinates for plot
Plot%XCoor(:)      = this%XCell(:)
Plot%ZCoor(:)      = this%ZCell(:)
Plot%CellSlope(:)  = this%CellSlope(:)
Plot%IndexSize     = Geometry%IndexSize

call Plot%plot(ModelInfo)

write(*,       *) " end subroutine < Discretize_1D_sub >"
write(FileInfo,*) " end subroutine < Discretize_1D_sub >"

return
end subroutine Discretize_network_sub

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


!#####################################
function Domain_Func_1D_MacDonald(x) result(Bathymetry)

implicit none

real(kind=Dbl)  :: x
real(kind=Dbl)  :: Bathymetry

real(kind=Dbl)  :: a1 = 0.674202
real(kind=Dbl)  :: a2 = 21.7112
real(kind=Dbl)  :: a3 = 14.492
real(kind=Dbl)  :: a4 = 1.4305

! code ============================================================================================

  if (0.0<= x .and. x <= 200.0_dbl/3.0_dbl) then
    Bathymetry = ((4.0_dbl/Gravity)**(1.0_dbl/3.0_dbl)) * ( 4.0_dbl/3.0_dbl - x/100.0_dbl ) &
               - (9*x/1000.0_dbl) * ( x/100.0_dbl - 2.0_dbl/3.0_dbl)
  else if (200.0_dbl/3.0_dbl <= x .and. x <= 100.0_dbl) then
    Bathymetry = ((4.0_dbl/Gravity)**(1.0_dbl/3.0_dbl))*( &
                 +a1 * (x/100.0_dbl - 2.0_dbl/3.0_dbl)**4 &
                 +a1 * (x/100.0_dbl - 2.0_dbl/3.0_dbl)**3 &
                 -a2 * (x/100.0_dbl - 2.0_dbl/3.0_dbl)**2 &
                 +a3 * (x/100.0_dbl - 2.0_dbl/3.0_dbl) &
                 + a4 &
                 )
  else
    write(*,*)" The defined bathymetry in the discretization_mod is wrong.",x
    stop
  end if

end function Domain_Func_1D_MacDonald

!##################################################################################################
function Domain_Func_1D_MacDonald_D(x) result(DBathymetry)

implicit none

real(kind=Dbl) :: x
real(kind=Dbl) :: DBathymetry

real(kind=Dbl)  :: a1 = 0.674202
real(kind=Dbl)  :: a2 = 21.7112
real(kind=Dbl)  :: a3 = 14.492
real(kind=Dbl)  :: a4 = 1.4305

! code ============================================================================================

  if (0.0<= x .and. x <= 200.0_dbl/3.0_dbl) then
    DBathymetry = ((4.0_dbl/Gravity)**(1.0_dbl/3.0_dbl)) * ( - 1.0_dbl/100.0_dbl ) &
               - (9/1000.0_dbl) * ( x/100.0_dbl - 2.0_dbl/3.0_dbl) &
               - (9*x/1000.0_dbl) * ( 1.0_dbl/100.0_dbl)
  else if (200.0_dbl/3.0_dbl <= x .and. x <= 100.0_dbl) then
    DBathymetry = ((4.0_dbl/Gravity)**(1.0_dbl/3.0_dbl))*( &
                 +4.0_dbl * a1 * (1.0_dbl/100.0_dbl) * (x/100.0_dbl - 2.0_dbl/3.0_dbl)**3 &
                 +3.0_dbl * a1 * (1.0_dbl/100.0_dbl) * (x/100.0_dbl - 2.0_dbl/3.0_dbl)**2 &
                 -2.0_dbl * a2 * (1.0_dbl/100.0_dbl) * (x/100.0_dbl                  )**1 &
                 +a3 * (1.0_dbl/100.0_dbl) &
                 )
  else
    write(*,*)" The defined bathymetry in the discretization_mod is wrong.",x
    stop
  end if

end function Domain_Func_1D_MacDonald_D

end module Discretization_mod
