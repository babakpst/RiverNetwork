
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
!
! File version $Id $
!
! Last update: 02/28/2018
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

implicit none

!Public
!Private

  interface Discretize
    module procedure Discretize_1D_sub
  end interface

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
!
! File version $Id $
!
! Last update: 02/28/2018
!
! ================================ L O C A L   V A R I A B L E S ==================================
! (Refer to the main code to see the list of imported variables)
!  . . . . . . . . . . . . . . . . variables . . . . . . . . . . . . . . . . . . . . . . . . . . .
!
!##################################################################################################

subroutine Discretize_1D_sub(                                         &
!                                                                     & ! integer (1) variables
!                                                                     & ! integer (2) variables
!                                                                     & ! integer (4) variables
!                                                                     & ! integer (8) variables
!                                                                     & ! real variables
!                                                                     & ! integer arrays
!                                                                     & ! real arrays
!                                                                     & ! characters
Geometry, InitialInfo, Discretization                                 & ! type
)


! Libraries =======================================================================================



! User defined modules ============================================================================
use Results_mod

implicit none

! Global variables ================================================================================

! - integer variables -----------------------------------------------------------------------------
!#integer (kind=Shrt), intent(in)    ::
!#integer (kind=Shrt), intent(inout) ::
!#integer (kind=Shrt), intent(out)   ::
! - real variables --------------------------------------------------------------------------------
!#real (kind=Dbl),     intent(in)    ::
!#real (kind=Dbl),     intent(inout) ::
!#real (kind=Dbl),     intent(out)   ::
! - complex variables -----------------------------------------------------------------------------
!#complex,             intent(in)    ::
!#complex,             intent(inout) ::
!#complex,             intent(out)   ::
! - integer Arrays --------------------------------------------------------------------------------
!#integer (kind=Shrt), intent(in),    dimension (:  )  ::
!#integer (kind=Shrt), intent(in),    dimension (:,:)  ::
!#integer (kind=Shrt), intent(in)    ::
!#integer (kind=Shrt), intent(inout) ::
!#integer (kind=Shrt), intent(out)   ::
! - real Arrays -----------------------------------------------------------------------------------
!#real (kind=Dbl),     intent(in),    dimension (:  )  ::
!#real (kind=Dbl),     intent(inout), dimension (:  )  ::
!#real (kind=Dbl),     intent(out),   dimension (:  )  ::
! - character variables ---------------------------------------------------------------------------
!#character (kind = ?, Len = ? ) ::
! - logical variables -----------------------------------------------------------------------------
!#logical   ::
! - types -----------------------------------------------------------------------------------------
type(Geometry_tp),       intent(in)  :: Geometry
type(InitialData_tp),    intent(in)  :: InitialInfo
type(discretization_tp), intent(out) :: Discretization

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

! - complex variables -----------------------------------------------------------------------------
!#complex              ::
! - integer Arrays --------------------------------------------------------------------------------
!#integer (kind=Shrt), dimension (:)  ::
!#integer (kind=Shrt), Allocatable, dimension (:)  ::
! - real Arrays -----------------------------------------------------------------------------------
!#real (kind=Dbl), dimension (:)      ::
!#real (kind=Dbl), allocatable, dimension (:)  ::
! - character variables ---------------------------------------------------------------------------
!#character (kind = ?, Len = ? ) ::
! - logical variables -----------------------------------------------------------------------------
!#logical   ::
! - type ------------------------------------------------------------------------------------------
type(Plot_domain_1D_tp) :: Plot ! Plots the discretized domain

! code ============================================================================================
write(*,       *) " subroutine <Discretize_1D>: "
write(FileInfo,*) " subroutine <Discretize_1D>: "

write(*,       *) " -Discretizing the domain ..."
write(FileInfo,*) " -Discretizing the domain ..."

! Find total number of cells in the domain:
Discretization%NCells = 0_Lng

write(*,fmt="(A)") " Calculating the total number of the cells in the domain ... "

  do i_reach = 1_Lng,InitialInfo%NoReaches
    Discretization%NCells = Discretization%NCells + Geometry%ReachDisc(i_reach)
  end do


write(*,fmt="(A,I15)") " Total number of cells: ", Discretization%NCells

allocate(Discretization%LengthCell(Discretization%NCells),              &
         Discretization%SlopeCell(Discretization%NCells),               &
         Discretization%ZCell(Discretization%NCells),                   &
         Discretization%ZFull(Discretization%NCells*2_Sgl + 1_Sgl),     &
         Discretization%ManningCell(Discretization%NCells),             &
         Discretization%WidthCell(Discretization%NCells),               &
         Discretization%X_Disc(Discretization%NCells),                  &
         Discretization%X_Full(Discretization%NCells*2_Sgl + 1_Sgl),    &
        stat=ERR_Alloc)

  if (ERR_Alloc /= 0) then
    write (*, Fmt_ALLCT) ERR_Alloc;  write (FileInfo, Fmt_ALLCT) ERR_Alloc;
    write(*, Fmt_FL);  write(FileInfo, Fmt_FL); read(*, Fmt_End);  stop;
  end if

write(*,fmt="(A)")" Loop over reaches to discretize the domain ..."

! Finding the highest point in the domain:
write(*,fmt="(A)")" Calculating the highest point in the domain ... "
MaxHeight = 0.0_Dbl

  do i_reach = 1_Lng,InitialInfo%NoReaches
    MaxHeight = MaxHeight + Geometry%ReachSlope(i_reach) * Geometry%ReachLength(i_reach)
  end do



write(*,fmt="(A,F23.10)") " Maximum height is:", MaxHeight

write(*,fmt="(A)")" Basic calculations ..."

CellCounter = 0_Lng
  do i_reach = 1_Lng, InitialInfo%NoReaches
    if (Geometry%ReachType(i_reach)==0) then
      CntrlVolumeLength =  floor(Geometry%ReachLength(i_reach)*1.0E10/Geometry%ReachDisc(i_reach) )/1.0E10     ! Control volume length
      write(*,fmt="(A,I5,A,F23.10)") " Cell length in the reach ", i_reach," is:", CntrlVolumeLength
      Height = MaxHeight
      Z_loss = floor(1.0E10*CntrlVolumeLength * Geometry%ReachSlope(i_reach))/ 1.0E10
      Height = Height +  floor(1.0E10 * 0.5_Dbl * Z_loss)/1.0E10
      TotalLength = 0.0_Dbl
      XCoordinate = 0.5_Dbl * CntrlVolumeLength

        do jj = 1_Lng,i_reach
          XCoordinate = XCoordinate + Geometry%ReachLength(jj)
        end do

        do jj = 1_Lng, Geometry%ReachDisc(i_reach) - 1_Lng
            Discretization%LengthCell(CellCounter)  = CntrlVolumeLength
            Discretization%X_Disc(CellCounter)      = XCoordinate
            Discretization%X_Full(CellCounter)      = XCoordinate - 0.5_Dbl * CntrlVolumeLength
            Discretization%X_Full(CellCounter*2_Lng+1_Lng) = XCoordinate

            XCoordinate = XCoordinate + CntrlVolumeLength
            TotalLength = TotalLength + CntrlVolumeLength
            Height      = Height - Z_loss

            Discretization%SlopeCell(CellCounter)  = Geometry%ReachSlope(i_reach)
            Discretization%ZCell(CellCounter)      = Height
            Discretization%ZFull(CellCounter*2)    = Height + 0.5_Dbl * Z_loss
            Discretization%ZFull(CellCounter*2+1)  = Height
            Discretization%ManningCell(CellCounter)= Geometry%ReachManning(i_reach)
            Discretization%WidthCell(CellCounter)  = Geometry%ReachWidth(i_reach)

            CellCounter = CellCounter + 1_Lng
        end do

      Discretization%LengthCell(CellCounter) = Geometry%ReachLength(i_reach) - TotalLength

      XCoordinate                            = XCoordinate - 0.5_dbl * CntrlVolumeLength + 0.5_dbl * Discretization%LengthCell(CellCounter)

      Discretization%X_Disc(CellCounter)     = XCoordinate
      Discretization%X_Full(CellCounter*2_Lng)       = XCoordinate - 0.5 * Discretization%LengthCell(CellCounter)
      Discretization%X_Full(CellCounter*2_Lng+1_Lng) = XCoordinate
      Discretization%X_Full(CellCounter*2_Lng+2_Lng) = XCoordinate + 0.5 * Discretization%LengthCell(CellCounter)

      Height = Height - (0.5_dbl * Z_loss + 0.5_dbl * Discretization%LengthCell(CellCounter) * Geometry%ReachSlope(i_reach))

      Discretization%ZCell(CellCounter)             = Height

      Discretization%ZFull(CellCounter*2_Lng)       = Height + 0.5_dbl * Discretization%LengthCell(CellCounter) * Geometry%ReachSlope(i_reach)
      Discretization%ZFull(CellCounter*2_Lng+1_Lng) = Height
      Discretization%ZFull(CellCounter*2_Lng)       = Height - 0.5_dbl * Discretization%LengthCell(CellCounter) * Geometry%ReachSlope(i_reach)
      Discretization%ManningCell(CellCounter)= Geometry%ReachManning(i_reach)
      Discretization%WidthCell(CellCounter)  = Geometry%ReachWidth(i_reach)


      CellCounter = CellCounter + 1_Lng
      MaxHeight   =  MaxHeight - Geometry%ReachLength(i_reach) * Geometry%ReachSlope(i_reach)

    else if (Geometry%ReachType(i_reach)==0) then
      Height = MaxHeight

      ProjectionLength = floor(1.0E10 * Geometry%ReachLength(i_reach) / Geometry%ReachDisc(i_reach) )/1.0E10

      XCoordinate = 0.5_Dbl * ProjectionLength

        do jj = 1_Lng,i_reach
          XCoordinate = XCoordinate + Geometry%ReachLength(jj)
        end do

        do jj = 1_Lng, Geometry%ReachDisc(i_reach)
          Z_loss = Domain_Func_1D(XCoordinate)

          Discretization%LengthCell(CellCounter) = dsqrt(ProjectionLength**2 + Z_loss**2)
          Discretization%X_Disc(CellCounter) = XCoordinate
          Discretization%X_Full(CellCounter*2_Lng)       = XCoordinate - 0.5_Dbl * ProjectionLength
          Discretization%X_Full(CellCounter*2_Lng+1_Lng) = XCoordinate

          Discretization%SlopeCell(CellCounter)  = Domain_Func_1D_D(XCoordinate)
          Discretization%ZCell(CellCounter)      = Height + Z_loss
          Discretization%ZFull(CellCounter*2)    = Height + Domain_Func_1D(XCoordinate - 0.5_Dbl * ProjectionLength)
          Discretization%ZFull(CellCounter*2+1)  = Height + Z_loss
          Discretization%ManningCell(CellCounter)= Geometry%ReachManning(i_reach)
          Discretization%WidthCell(CellCounter)  = Geometry%ReachWidth(i_reach)

          XCoordinate = XCoordinate + ProjectionLength
          TotalLength = TotalLength + ProjectionLength
          CellCounter = TotalLength + 1_Lng
        end do

      MaxHeight = MaxHeight - Geometry%ReachLength(i_reach) * Geometry%ReachSlope(i_reach)
    end if

  end do


  if (Discretization%NCells /= CellCounter) then
    write(*,        fmt = "(A)") "Fatal error: Mismatch between the number of cells. Check the discretization module."
    write(FileInfo, fmt = "(A)") "Fatal error: Mismatch between the number of cells. Check the discretization module."
    write(*, Fmt_End); read(*,*); stop;
  end if


! Plot the discretized domain (cell centers)
Plot%NPoints = Discretization%NCells
Plot%ModelName =
Plot%OutputDir =
Plot%AnalysisOutputDir =
Plot%

allocate(Plot%XCoor(Plot%NPoints), Plot%ZCoor(Plot%NPoints), stat=ERR_Alloc)

  if (ERR_Alloc /= 0) then
    write (*, Fmt_ALLCT) ERR_Alloc;  write (UnInf, Fmt_ALLCT) ERR_Alloc;
    write(*, Fmt_FL);  write(UnInf, Fmt_FL); read(*, Fmt_End);  stop;
  end if

call


! Plot full results



write(*,       *) " -Domain discretized successfully."
write(FileInfo,*) " -Domain discretized successfully."

write(*,       *) " end subroutine <Discretize_1D>"
write(FileInfo,*) " end subroutine <Discretize_1D>"
end subroutine Discretize_1D_sub
return



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

function Domain_Func_1D_D(x) result(DBathymetry)

implicit none

real(kind=Dbl) :: x
real(kind=Dbl) :: DBathymetry



! code ============================================================================================

DBathymetry = - 0.05_Dbl * 2.0_Dbl * (x-10.0_Dbl)

end function Domain_Func_1D_D




end module Discretization_mod
