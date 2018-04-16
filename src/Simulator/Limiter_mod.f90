
!##################################################################################################
! Purpose: This module solves the shallow water equations using a combination of upwind method with
!          Lax-Wendroff method, using a limiter. This module contains all other requirements,
!          limiter, boundary conditions, etc.
!
! Developed by: Babak Poursartip
! Supervised by: Clint Dawson
!
! The Institute for Computational Engineering and Sciences (ICES)
! The University of Texas at Austin
!
! ================================ V E R S I O N ==================================================
! V0.00: 03/15/2018 - File initiated.
! V0.01: 03/24/2018 - Initiated: Compiled without error for the first time.
!
! File version $Id $
!
! Last update: 03/27/2018
!
! ================================ S U B R O U T I N E ============================================
! - Solver_1D_with_Limiter_sub: Solves the 1D shallow water equation, with limiter.
! - Impose_Boundary_Condition_1D_sub: Imposes boundary conditions on the 1D model.
! - Limiters_sub: Contains various limiters.
! - Jacobian_sub: computes the Jacobian matrix, Jacobian plus, and Jacobian minus at each cell.
! - Eigenvalues_sub: computes the eigenvalues of a 2x2 matrix.
!
! ================================ F U N C T I O N ================================================
!
! ================================ L O C A L   V A R I A B L E S ==================================
! (Refer to the main code to see the list of imported variables)
!  . . . . . . . . . . . . . . . . Variables . . . . . . . . . . . . . . . . . . . . . . . . . . .
!
!##################################################################################################

module LaxWendroff_with_limiter_mod

! Libraries =======================================================================================


! User defined modules ============================================================================
use Parameters_mod
use Results_mod
use Discretization_mod

implicit none

!public
!private

type LimiterFunc_tp ! contains all variable to compute the limiter value
  integer(kind=Smll) :: limiter_Type ! Indicates the type of limiter function

  real(kind=Dbl)     :: phi          ! the value of limiter
  real(kind=Dbl)     :: theta        ! the argument of the limiter

  contains
    procedure LimiterValue => Limiters_sub
end type LimiterFunc_tp

! This vector will be used in the main type as the solution in each type step
!type vector
!  real(kind=Dbl), dimension(2) :: U
!end type vector

! This type consists of all variables/arrays regarding the Jacobian, used to apply the limiter. The input variables is U_up and U_dw
type Jacobian_tp
  integer(kind=Tiny) :: option   ! indicates how to interpolate the Jacobian at the interface: 1: for average on the solution-2: direct average on the Jacobian itself

  real(kind=Dbl),dimension(2,2) :: A        ! Contains the Jacobian matrix at each time step at the cell interface i-1/2
  real(kind=Dbl),dimension(2,2) :: R        ! Contains the eigenvectors at each time step at the cell interface i-1/2
  real(kind=Dbl),dimension(2,2) :: L        ! Contains the eigenvectors inverse (R^(-1))at each time step at the cell interface i-1/2

  real(kind=Dbl),dimension(2,2) :: A_plus   ! Contains the Jacobian matrix with + eigenvalues at each time step at the cell interface i-1/2
  real(kind=Dbl),dimension(2,2) :: A_minus  ! Contains the Jacobian matrix with - eigenvalues at each time step at the cell interface i-1/2
  real(kind=Dbl),dimension(2,2) :: A_abs    ! Contains the Jacobian matrix with abs eigenvalues at each time step at the cell interface i-1/2
  real(kind=Dbl),dimension(2,2) :: Gam      ! Contains the Jacobian matrix with - eigenvalues at each time step at the cell interface i-1/2
  real(kind=Dbl),dimension(2,2) :: Gam_plus ! Contains the Jacobian matrix with - eigenvalues at each time step at the cell interface i-1/2
  real(kind=Dbl),dimension(2,2) :: Gam_minus! Contains the Jacobian matrix with - eigenvalues at each time step at the cell interface i-1/2

  type(vector) :: U_up, U_dw ! Holds the solution at the upstream and downstream of each cell

  type(vector) :: Lambda       ! Contains the eigenvalues
  type(vector) :: Lambda_plus  ! Contains the positive eigenvalues
  type(vector) :: Lambda_minus ! Contains the negative eigenvalues

  contains
    procedure Jacobian => Jacobian_sub
end type Jacobian_tp

! Contains the parameters for considering the source term within the solution.
type SoureceTerms_tp
  real(kind=Dbl)  :: S_f ! friction slope
  real(kind=Dbl)  :: S_f_interface ! friction slope at the interface

  type(vector)  :: S      ! source term
  type(vector)  :: S_interface      ! source term at the interface

  type(vector)  :: Source_1 ! contribution of the source term in updating the solution
  type(vector)  :: Source_2 ! contribution of the source term in updating the solution

  real(kind=Dbl), dimension(2,2) :: B  ! This is in fact dS / dU
  real(kind=Dbl), dimension(2,2) :: BI ! B inverse, see notes
  real(kind=Dbl), dimension(2,2) :: Identity ! Identity matrix
end type SoureceTerms_tp


! Contains the parameters for the solution
type, public :: SolverWithLimiter(NCells)
  integer(kind=Lng), len :: NCells
  integer(kind=Lng)      :: Plot_Inc = 100

  type(vector), dimension(NCells) :: S     ! Source term

  type(vector), dimension(NCells*2) :: phi   ! Holds the value of the limiter function <delete> <modify>
  type(vector), dimension(NCells*2) :: theta ! Holds the value of the limiter function <delete> <modify>
  type(vector), dimension(-1_Lng:NCells+2_Lng)  :: U ! This vector holds the solution at the current step,
                                            ! the first term holds "h" and the second holds "uh"
  type(discretization_tp) :: Discretization ! Contains the discretization of the domain
  type(AnalysisData_tp)   :: AnalysisInfo   ! Holds information for the analysis
  type(Input_Data_tp)     :: ModelInfo      ! Holds information for the model

  contains
    procedure Solve => Solver_1D_with_Limiter_sub
    procedure BC => Impose_Boundary_Condition_1D_sub
end type SolverWithLimiter

contains

!##################################################################################################
! Purpose: This module solves the shallow water equations using a combination of upwind method with
!          Lax-Wendroff method, using a limiter.
!
! Developed by: Babak Poursartip
! Supervised by: Clint Dawson
!
! The Institute for Computational Engineering and Sciences (ICES)
! The University of Texas at Austin
!
! ================================ V E R S I O N ==================================================
! V0.00: 03/15/2018 - File initiated.
! V0.01: 03/24/2018 - Initiated: Compiled without error for the first time.
!
! File version $Id $
!
! Last update: 03/24/2018
!
! ================================ L O C A L   V A R I A B L E S ==================================
! (Refer to the main code to see the list of imported variables)
!  . . . . . . . . . . . . . . . . Variables . . . . . . . . . . . . . . . . . . . . . . . . . . .
!
!##################################################################################################

subroutine Solver_1D_with_Limiter_sub(this)

! Libraries =======================================================================================

! User defined modules ============================================================================

implicit none

! Global variables ================================================================================

! - types -----------------------------------------------------------------------------------------
class(SolverWithLimiter(*)) :: this

! Local variables =================================================================================
! - integer variables -----------------------------------------------------------------------------
integer(kind=Lng)  :: i_Cell    ! loop index over the Cells
integer(kind=Lng)  :: i_steps   ! loop index over the number steps
integer(kind=Lng)  :: NSteps    ! Total number of steps for time marching

integer(kind=Tiny)  :: i_eigen     ! loop index over the eigenvalues (only two in case of 1D SWE)
integer(kind=Tiny)  :: i_Interface ! loop index over the interfaces (only two in case of 1D SWE)


! - real variables --------------------------------------------------------------------------------
real(kind=Dbl)      :: dt      ! time step, should be structured/constant in each reach
real(kind=Dbl)      :: dx      ! cell length, should be structured/constant in each reach
real(kind=Dbl)      :: speed   ! characteristic speed, equal to positive or negative eiqenvalues in each interface
real(kind=Dbl)      :: dtdx    ! The ratio dt/dx, used in the final equation
real(kind=Dbl)      :: Coefficient ! This will take care of the sign of the flux for the high-resolution part

real(kind=Dbl)      :: height   ! height of water at the current cell/time
real(kind=Dbl)      :: velocity ! velocity of water at the current cell/time

real(kind=Dbl)      :: height_interface   ! height of water at the current interface/time
real(kind=Dbl)      :: velocity_interface ! velocity of water at the current interface/time

! - real Arrays -----------------------------------------------------------------------------------
type(vector) :: TempSolution

! - logical variables -----------------------------------------------------------------------------
logical   :: PrintResults

! - type ------------------------------------------------------------------------------------------
type(Jacobian_tp)      :: Jacobian ! Contains the Jacobian and all related items.
type(Jacobian_tp)      :: Jacobian_neighbor ! Contains the Jacobian and all related items.
type(LimiterFunc_tp)   :: LimiterFunc ! Contains the values of the limiter
type(Plot_Results_1D_limiter_tp(NCells = :)), allocatable :: Results ! Holds the results in each time step in all cells.
type(SoureceTerms_tp) :: SourceTerms

type(vector) :: alpha                 ! alpha = R^-1 (U_i- U_i-1) See notes for detail.
type(vector) :: alpha_neighbor        ! alpha = R^-1 (U_i- U_i-1) See notes for detail.
type(vector) :: alpha_tilda           ! alpha_tilda = alpha * limiter

type(vector) :: Wave                 ! Wave = alpha * R
type(vector) :: Wave_neighbor        ! Wave = alpha * R
type(vector) :: Wave_tilda           ! Wave = alpha_tilda * R

type(vector) :: F_L ! Contribution of low-resolution method (Upwind) in the solution.
type(vector) :: F_H ! Contribution of high-resolution method (Lax-Wendroff) in the solution.

type(vector) :: Delta_U           ! holds (U_i- U_i-1)

! code ============================================================================================
write(*,       *) " subroutine < Solver_1D_with_Limiter_sub >: "
write(FileInfo,*) " subroutine < Solver_1D_with_Limiter_sub >: "

write(*,       *) " -Solving the shallow water equation with a limiter ..."
write(FileInfo,*) " -Solving the shallow water equation with a limiter ..."

! Applying initial conditions:
write(*,       *) " -Applying initial conditions ..."
write(FileInfo,*) " -Applying initial conditions ..."

allocate(Plot_Results_1D_limiter_tp(NCells = this%NCells) :: Results)

this%U(1:this%NCells)%U(1) = this%AnalysisInfo%CntrlV -    this%Discretization%ZCell(:)
this%U(0)%U(1)  = this%U(1)%U(1)
this%U(-1)%U(1) = this%U(1)%U(1)

this%U(this%NCells+1)%U(1) = this%U(this%NCells)%U(1)
this%U(this%NCells+2)%U(1) = this%U(this%NCells)%U(1)

this%U(:)%U(2) = 0.0_Dbl


this%S(:)%U(1)     = 0.0_Dbl
this%S(:)%U(2)     = 0.0_Dbl
this%phi(:)%U(1)   = 0.0_Dbl
this%phi(:)%U(2)   = 0.0_Dbl
this%theta(:)%U(1) = 0.0_Dbl
this%theta(:)%U(2) = 0.0_Dbl




NSteps = this%AnalysisInfo%TotalTime/this%AnalysisInfo%TimeStep
dt     = this%AnalysisInfo%TimeStep
dx     = this%Discretization%LengthCell(1)

dtdx = dt / dx

! <modify>
Jacobian%option = 1
Jacobian_neighbor%option = 1

! Initialization
LimiterFunc%limiter_Type = this%AnalysisInfo%limiter ! Define what limiter to use in the algorithm
!PrintResults = .true.
PrintResults = .false.
SourceTerms%Identity(:,:) = 0.0_Dbl
SourceTerms%Identity(1,1) = 1.0_Dbl
SourceTerms%Identity(2,2) = 1.0_Dbl

call this%BC()
Results%ModelInfo = this%ModelInfo

  ! Time marching
  Time_Marching: do i_steps = 1_Lng, NSteps

    print*, "----------------Step:", i_steps

    ! write down data for visualization
      if (mod(i_steps,this%Plot_Inc)==1 .or. PrintResults) then
        Results%U(:)    = this%U(1:this%NCells)
        Results%s(:)    = this%S(:)
        Results%phi(:)  = this%phi(:)
        Results%theta(:)= this%theta(:)

        call Results%plot_results(i_steps)
      end if

      ON_Cells: do i_Cell = 2_Lng,this%NCells-1  ! Loop over the cells except the boundary cells.

        !print*, "=============Cell:", i_Cell

        ! Initialize fluxes
        F_L%U(:) = 0.0_Dbl ! upwind flux (not exactly, see notes)
        F_H%U(:) = 0.0_Dbl ! lax-Wendroff flux (not exactly, see notes)

        SourceTerms%Source_1%U(:)  = 0.0_Dbl
        SourceTerms%Source_2%U(:)  = 0.0_Dbl

        ! Solution at this cell
        height   = this%U(i_Cell)%U(1)
        velocity = this%U(i_Cell)%U(2) / height

        ! Find the B matrix for this cell
        SourceTerms%S_f = (this%Discretization%ManningCell(i_Cell)**2.0)  * velocity * dabs(velocity) /( height**(4.0_Dbl/3.0_Dbl) )

        SourceTerms%B(1,1) = 0.0_Dbl
        SourceTerms%B(1,2) = 0.0_Dbl
        SourceTerms%B(2,1) = - Gravity * ( this%Discretization%SlopeCell(i_Cell) + (7.0_Dbl/3.0_Dbl) * SourceTerms%S_f  )
        SourceTerms%B(2,2) =   (2.0_Dbl * this%Discretization%ManningCell(i_Cell)**2.0)  * dabs(velocity) /( height**(4.0_Dbl/3.0_Dbl) )

        !print*,"B: ",i_Cell, SourceTerms%B

        ! Find the BI
        SourceTerms%BI(:,:) = SourceTerms%Identity - 0.5_Dbl * dt * SourceTerms%B(:,:)
        !print*,"BI before",SourceTerms%BI
        call Inverse(SourceTerms%BI(:,:), SourceTerms%BI(:,:)) ! the inverse
        !print*,"BI ",SourceTerms%BI
        ! Source terms at the current cell/time
        SourceTerms%S%U(1) = 0.0_Dbl
        SourceTerms%S%U(2) = - Gravity * height  * ( this%Discretization%SlopeCell(i_Cell) - SourceTerms%S_f )

        ! The first contribution of the source term in the solution
        SourceTerms%Source_1%U(:) = dt * ( SourceTerms%S%U(:)  - 0.5_Dbl * matmul( SourceTerms%B(:,:), this%U(i_Cell)%U(:) )  )

          ON_Interface:  do i_Interface = 1, 2  ! the first one is on i-1/2, and the second one is on i+1/2

            ! Compute the jump (U_i- U_i-1)
            Delta_U%U(:) = this%U(i_Cell+(i_Interface-1_Lng))%U(:) - this%U(i_Cell+(i_Interface-2_Lng))%U(:)

            ! Computing the Jacobian and all other items at the upstream
            Jacobian%U_up%U(:) = this%U(i_Cell+(i_Interface-2_Lng))%U(:)
            Jacobian%U_dw%U(:) = this%U(i_Cell+(i_Interface-1_Lng))%U(:)

            call Jacobian%Jacobian( i_eigen,i_Interface,i_Cell )   ! <modify>

            ! Compute alpha(= RI*(U_i - U_(i-1))
            alpha%U(:) = matmul(Jacobian%L, Delta_U%U(:))

            ! Source terms
            if (i_Interface == 1_Tiny) then
              Coefficient = -1.0_Dbl               ! This will take care of the sign of the flux for the high-resolution part
            else if (i_Interface == 2_Tiny) then
              Coefficient = +1.0_Dbl                ! This will take care of the sign of the flux for the high-resolution part
            end if

            height_interface = 0.5_Dbl * (Jacobian%U_up%U(1) +Jacobian%U_dw%U(1) )
            velocity_interface = 0.5_Dbl * (Jacobian%U_up%U(2)/Jacobian%U_up%U(1) + Jacobian%U_dw%U(2)/Jacobian%U_dw%U(1) )

            SourceTerms%S_f_interface = this%Discretization%ManningCell(i_Cell)  * velocity_interface * dabs(velocity_interface) /( height_interface**(4.0_Dbl/3.0_Dbl) )

            SourceTerms%S_interface%U(1) = 0.0_Dbl
            SourceTerms%S_interface%U(2) = - Gravity * height_interface  * ( this%Discretization%SlopeInter(i_Cell + i_Interface-1_Tiny ) - SourceTerms%S_f_interface )

            SourceTerms%Source_2%U(:) = SourceTerms%Source_2%U(:) + 0.5_Dbl * (dt**2) / dx * ( Coefficient * matmul( Jacobian%A, SourceTerms%S_interface%U(:)) )

            if ( alpha%U(1) ==0.0_Dbl .and. alpha%U(2) ==0.0_Dbl) cycle

              ON_Eigenvalues: do i_eigen = 1_Tiny, 2_Tiny

                Wave%U(:) = alpha%U(i_eigen) * Jacobian%R(:,i_eigen)

                  if (i_Interface == 1_Tiny) then ! we use the positive eigenvalues on the upstream interface
                    speed = Jacobian%Lambda_plus%U(i_eigen)
                    Coefficient = -1.0_Dbl               ! This will take care of the sign of the flux for the high-resolution part
                  else if (i_Interface == 2_Tiny) then ! we use the negative eigenvalues on the downstream interface
                    speed = Jacobian%Lambda_minus%U(i_eigen)
                    Coefficient = +1.0_Dbl                ! This will take care of the sign of the flux for the high-resolution part
                  end if

                ! The upwind part
                F_L%U(:) = F_L%U(:) + speed * Wave%U(:)

                  ! This if condition computes the W_(I-1/2)
                  if  (Jacobian%Lambda%U(i_eigen)  > 0.0_Dbl ) then

                    ! Compute the jump (U_i- U_i-1)
                    Delta_U%U(:) = this%U(i_Cell+i_Interface-2_Tiny)%U(:) - this%U( i_Cell+i_Interface-3_Tiny )%U(:)

                    ! Computing the Jacobian and all other items at the upstream
                    Jacobian_neighbor%U_up%U(:) = this%U( i_Cell+i_Interface-3_Tiny  )%U(:)
                    Jacobian_neighbor%U_dw%U(:) = this%U( i_Cell+i_Interface-2_Tiny  )%U(:)

                    call Jacobian_neighbor%Jacobian(i_eigen,i_Interface,i_Cell)   ! <modify>

                    ! Compute alpha(= RI*(U_i - U_(i-1))
                    alpha_neighbor%U(:) = matmul(Jacobian_neighbor%L(:,:), Delta_U%U(:))
                    Wave_neighbor%U(:)  = alpha_neighbor%U(i_eigen) * Jacobian_neighbor%R(:, i_eigen)

                  else if  ( Jacobian%Lambda%U(i_eigen)  < 0.0_Dbl ) then

                   ! Compute the jump (U_i- U_i-1)
                    Delta_U%U(:) = this%U( i_Cell+i_Interface )%U(:) - this%U( i_Cell+i_Interface-1_Tiny )%U(:)

                    ! Computing the Jacobian and all other items at the upstream
                    Jacobian_neighbor%U_up%U(:) = this%U(i_Cell+i_Interface-1_Tiny )%U(:)
                    Jacobian_neighbor%U_dw%U(:) = this%U(i_Cell+i_Interface        )%U(:)

                    call Jacobian_neighbor%Jacobian(i_eigen,i_Interface,i_Cell)   ! <modify>

                    ! Compute alpha(= RI*(U_i - U_(i-1))
                    alpha_neighbor%U(:) = matmul(Jacobian_neighbor%L(:,:), Delta_U%U(:))
                    Wave_neighbor%U(:)  = alpha_neighbor%U(i_eigen) * Jacobian_neighbor%R(:,i_eigen)
                  else
                    write(*,*) " Something is wrong. Check the limiter subroutine."
                    stop
                  end if

                  if ( dot_product(Wave%U(:), Wave%U(:) ) /= 0.0_Dbl ) then
                    LimiterFunc%theta = ( dot_product( Wave_neighbor%U(:), Wave%U(:) ) ) / ( dot_product(Wave%U(:), Wave%U(:) )  )

                  else
                    LimiterFunc%theta = 0.0_Dbl
                  end if

                ! The limiter function
                call LimiterFunc%LimiterValue()

                !LimiterFunc%phi =0.0
                alpha_tilda%U(:) =  LimiterFunc%phi * alpha%U(:)

                this%theta (2*(i_Cell-1)+i_Interface)%U(i_eigen) = LimiterFunc%theta
                this%phi   (2*(i_Cell-1)+i_Interface)%U(i_eigen) = LimiterFunc%phi

                Wave_tilda%U(:) = alpha_tilda%U(i_eigen) * Jacobian%R(:,i_eigen)

                ! The high-resolution (Lax-Wendroff) part
                F_H%U(:) = F_H%U(:) + Coefficient * 0.5_Dbl * dabs(Jacobian%Lambda%U(i_eigen) ) * ( 1.0_Dbl - dtdx * dabs( Jacobian%Lambda%U(i_eigen) ) ) * Wave_tilda%U(:)

              end do ON_Eigenvalues
          end do ON_Interface

        ! Final update the results
        TempSolution%U(:) = this%U(i_cell)%U(:) - dtdx * F_L%U(:) - dtdx * F_H%U(:) + SourceTerms%Source_1%U(:) - SourceTerms%Source_2%U(:)

        this%U(i_cell)%U(:) = matmul(SourceTerms%BI(:,:), TempSolution%U(:))

      end do ON_Cells

    ! apply boundary condition
    call this%BC()

  end do Time_Marching

write(*,       *) " end subroutine < Solver_1D_with_Limiter_sub >"
write(FileInfo,*) " end subroutine < Solver_1D_with_Limiter_sub >"
return

end subroutine Solver_1D_with_Limiter_sub


!##################################################################################################
! Purpose: This subroutine imposes the boundary conditions on a 1D domain.
!
! Developed by: Babak Poursartip
! Supervised by: Clint Dawson
!
! The Institute for Computational Engineering and Sciences (ICES)
! The University of Texas at Austin
!
! ================================ V E R S I O N ==================================================
! V0.00: 03/15/2018 - File initiated.
! V0.01: 03/15/2018 - Initiated: Compiled without error for the first time.
!
! File version $Id $
!
! Last update: 03/15/2018
!
! ================================ L O C A L   V A R I A B L E S ==================================
! (Refer to the main code to see the list of imported variables)
!  . . . . . . . . . . . . . . . . Variables . . . . . . . . . . . . . . . . . . . . . . . . . . .
!
!##################################################################################################

subroutine Impose_Boundary_Condition_1D_sub(this)

! Libraries =======================================================================================

! User defined modules ============================================================================


implicit none

! Global variables ================================================================================

! - types -----------------------------------------------------------------------------------------
class(SolverWithLimiter(*)) :: this


! Local variables =================================================================================

! code ============================================================================================
!write(*,       *) " subroutine < Impose_Boundary_Condition_1D_sub >: "
!write(FileInfo,*) " subroutine < Impose_Boundary_Condition_1D_sub >: "

! Boundary conditions on the height
this%U(-1_Lng)%U(1) = this%U(2)%U(1) ! h at the upstream
this%U( 0_Lng)%U(1) = this%U(2)%U(1) ! h at the upstream
this%U( 1_Lng)%U(1) = this%U(2)%U(1) ! h at the upstream

this%U(this%NCells      )%U(1) = this%AnalysisInfo%h_dw ! h at the downstream
this%U(this%NCells+1_Lng)%U(1) = this%AnalysisInfo%h_dw ! h at the downstream
this%U(this%NCells+2_Lng)%U(1) = this%AnalysisInfo%h_dw ! h at the downstream

! Boundary conditions on the discharge
this%U(-1_Lng)%U(2) = this%AnalysisInfo%Q_Up / this%Discretization%WidthCell(1)  ! h at the upstream
this%U( 0_Lng)%U(2) = this%AnalysisInfo%Q_Up / this%Discretization%WidthCell(1)  ! h at the upstream
this%U( 1_Lng)%U(2) = this%AnalysisInfo%Q_Up / this%Discretization%WidthCell(1)  ! h at the upstream

this%U(this%NCells      )%U(2) = this%U(this%NCells-1)%U(2) ! h at the downstream
this%U(this%NCells+1_Lng)%U(2) = this%U(this%NCells-1)%U(2) ! h at the downstream
this%U(this%NCells+2_Lng)%U(2) = this%U(this%NCells-1)%U(2) ! h at the downstream

!write(*,       *) " end subroutine < Impose_Boundary_Condition_1D_sub >"
!write(FileInfo,*) " end subroutine < Impose_Boundary_Condition_1D_sub >"
return
end subroutine Impose_Boundary_Condition_1D_sub

!##################################################################################################
! Purpose: This subroutine contains various limiters functions.
!
! Developed by: Babak Poursartip
! Supervised by: Clint Dawson
!
! The Institute for Computational Engineering and Sciences (ICES)
! The University of Texas at Austin
!
! ================================ V E R S I O N ==================================================
! V0.00: 03/15/2018 - Subroutine initiated.
! V0.01: 03/15/2018 - Initiated: Compiled without error for the first time.
!
! File version $Id $
!
! Last update: 03/15/2018
!
! ================================ L O C A L   V A R I A B L E S ==================================
! (Refer to the main code to see the list of imported variables)
!  . . . . . . . . . . . . . . . . Variables . . . . . . . . . . . . . . . . . . . . . . . . . . .
!
!##################################################################################################

subroutine Limiters_sub(this)


! Libraries =======================================================================================

! User defined modules ============================================================================


implicit none

! Global variables ================================================================================
! - types -----------------------------------------------------------------------------------------
class(LimiterFunc_tp) :: this

! Local variables =================================================================================

! code ============================================================================================
!write(*,       *) " subroutine < Limiters_sub >: "
!write(FileInfo,*) " subroutine < Limiters_sub >: "

  select case (this%limiter_Type)

    case(1)  ! minmod: most diffusive
      this%phi = dmax1(  0.0_Dbl, dmin1( 1.0_Dbl, this%theta )  )
    case(2)  !superbee
      this%phi = dmax1( 0.0_Dbl, dmin1( 1.0_Dbl, 2.0_Dbl*this%theta ), dmin1( 2.0_Dbl, this%theta))
    case(3)  ! MC (Woodward)
      this%phi = dmax1( 0.0_Dbl, dmin1( (1.0_Dbl + this%theta)/2.0_Dbl, 2.0_Dbl, 2.0_Dbl*this%theta ) )
    case(4)  ! van Leer
      this%phi = ( this%theta +  dabs(this%theta) ) / ( 1.0_Dbl + dabs(this%theta) )
    case default
      write(*,*)" The limiter type does exist, please select a limiter from the list and modify the input file."
      write(FileInfo,*)" The limiter type does exist, please select a limiter from the list and modify the input file."
      write(*,*)
      write(FileInfo,*)
      write(*,*)" Simulation terminated with error."
      write(*, Fmt_End);  read(*,*);   stop;

  end select


!write(*,       *) " end subroutine < Limiters_sub >"
!write(FileInfo,*) " end subroutine < Limiters_sub >"
return
end subroutine Limiters_sub


!##################################################################################################
! Purpose: This subroutine computes the Jacobian matrix, Jacobian plus, and Jacobian minus at each cell.
!
! Developed by: Babak Poursartip
! Supervised by: Clint Dawson
!
! The Institute for Computational Engineering and Sciences (ICES)
! The University of Texas at Austin
!
! ================================ V E R S I O N ==================================================
! V0.00: 03/15/2018 - Subroutine initiated.
! V0.01: 03/15/2018 - Initiated: Compiled without error for the first time.
!
! File version $Id $
!
! Last update: 03/20/2018
!
! ================================ L O C A L   V A R I A B L E S ==================================
! (Refer to the main code to see the list of imported variables)
!  . . . . . . . . . . . . . . . . Variables . . . . . . . . . . . . . . . . . . . . . . . . . . .
!
!##################################################################################################

subroutine Jacobian_sub(this,i_eigen,i_Interface,i_Cell)


! Libraries =======================================================================================

! User defined modules ============================================================================


implicit none

! Global variables ================================================================================
! - types -----------------------------------------------------------------------------------------
class(Jacobian_tp) :: this

! Local variables =================================================================================

integer(kind=tiny) :: i_eigen,i_Interface !<delete>
integer(kind=Lng):: i_Cell !<delete>

real(kind=Dbl) :: h_dw  ! the height at the downstream grid
real(kind=Dbl) :: u_dw  ! the velocity at the downstream grid

real(kind=Dbl) :: h_up  ! the height at the upstream grid
real(kind=Dbl) :: u_up  ! the velocity at the upstream grid

real(kind=Dbl) :: u_ave   ! the average velocity at the interface
real(kind=Dbl) :: h_ave  ! the average height at the interface

real(kind=Dbl) :: c  ! wave speed

real(kind=Dbl), dimension(2,2) :: A_up  ! the average discharge at the interface
real(kind=Dbl), dimension(2,2) :: A_dw  ! the average discharge at the interface

! code ============================================================================================
!write(*,       *) " subroutine < Jacobian_sub >: "
!write(FileInfo,*) " subroutine < Jacobian_sub >: "

  if (this%option == 1 ) then  ! find the average solution at the interface and then compute the Jacobian

    h_dw = this%U_dw%U(1)
    u_dw = this%U_dw%U(2) / this%U_dw%U(1)

    h_up = this%U_up%U(1)
    u_up = this%U_up%U(2) / this%U_up%U(1)

    h_ave = 0.5_Dbl*(h_up+h_dw)    ! <modify> for unstructured discretization
    u_ave = 0.5_Dbl*(u_up+u_dw)   ! <modify> for unstructured discretization

    c = dsqrt (Gravity * h_ave) ! wave speed

    ! Computing the Jacobian - A
    this%A(1,1) = 0.0_Dbl
    this%A(1,2) = 1.0_Dbl
    this%A(2,1) = Gravity * h_ave - u_ave**2
    this%A(2,2) = 2.0_Dbl * u_ave

    ! Computing the eigenvalues
    this%Lambda%U(1) = u_ave - dsqrt(Gravity *  h_ave)
    this%Lambda%U(2) = u_ave + dsqrt(Gravity *  h_ave)
    !print*,"inside ", this%Lambda%U(1),i_eigen,i_Interface,i_Cell ! <delete>
    !print*,"inside ", this%Lambda%U(2),i_eigen,i_Interface,i_Cell  ! <delete>


    this%Lambda_plus%U(1) =  dmax1(this%Lambda%U(1), 0.0_Dbl)
    this%Lambda_plus%U(2) =  dmax1(this%Lambda%U(2), 0.0_Dbl)

    this%Lambda_minus%U(1) =  dmin1(this%Lambda%U(1), 0.0_Dbl)
    this%Lambda_minus%U(2) =  dmin1(this%Lambda%U(2), 0.0_Dbl)

    ! Computing the eigenvectors
    this%R(1,1) = 1.0_Dbl
    this%R(2,1) = this%Lambda%U(1)

    this%R(1,2) = 1.0_Dbl
    this%R(2,2) = this%Lambda%U(2)

    ! Computing the eigenvectors inverse
    this%L(1,1) = this%Lambda%U(2)
    this%L(1,2) = -1.0_Dbl

    this%L(2,1) = -this%Lambda%U(1)
    this%L(2,2) = 1.0_Dbl

    this%L(:,:)  =  this%L(:,:)   /(2.0_Dbl * c)

    ! Fill eigenvalue matrix
    this%Gam(1,1) = this%Lambda%U(1)
    this%Gam(1,2) = 0.0_Dbl
    this%Gam(2,1) = 0.0_Dbl
    this%Gam(2,2) = this%Lambda%U(2)

    ! Fill Gamma plus
    this%Gam_plus(1,1) = dmax1(this%Lambda%U(1), 0.0_Dbl)
    this%Gam_plus(1,2) = 0.0_Dbl
    this%Gam_plus(2,1) = 0.0_Dbl
    this%Gam_plus(2,2) = dmax1(this%Lambda%U(2), 0.0_Dbl)

    ! Fill Gamma minus
    this%Gam_minus(1,1) = dmin1(this%Lambda%U(1), 0.0_Dbl)
    this%Gam_minus(1,2) = 0.0_Dbl
    this%Gam_minus(2,1) = 0.0_Dbl
    this%Gam_minus(2,2) = dmin1(this%Lambda%U(2), 0.0_Dbl)

    ! Compute A plus
    this%A_plus  = matmul(matmul(this%R, this%Gam_plus), this%L)

    ! Compute A minus
    this%A_minus = matmul(matmul(this%R, this%Gam_minus), this%L)

    ! Compute A abs
    this%A_abs = this%A_plus - this%A_minus


  else if (this%option == 2 ) then  ! find the Jacobian at each grid and average the Jacobian to find the Jacobian at the interface

    ! Computing the Jacobian at the upstream - A
    h_up = this%U_up%U(1)
    u_up = this%U_up%U(2) / this%U_up%U(1)
    A_up(1,1) = 0.0_Dbl
    A_up(1,2) = 1.0_Dbl
    A_up(2,1) = Gravity * h_up - u_up**2
    A_up(2,2) = 2.0_Dbl * u_up


    ! Computing the Jacobian at the upstream - A
    h_dw = this%U_dw%U(1)
    u_dw = this%U_dw%U(2) / this%U_dw%U(1)
    A_dw(1,1) = 0.0_Dbl
    A_dw(1,2) = 1.0_Dbl
    A_dw(2,1) = Gravity * h_dw - u_dw**2
    A_dw(2,2) = 2.0_Dbl * u_dw

    ! average Jacobian
    this%A(:,:) = 0.5_Dbl * ( A_up(:,:) + A_dw(:,:) )

    ! Computing the eigenvalues
    call Eigenvalues_sub(this%A, this%Lambda%U(1), this%Lambda%U(2))

    ! Computing the eigenvectors
    this%R(1,1) = 1.0_Dbl
    this%R(2,1) = this%Lambda%U(1)

    this%R(1,2) = 1.0_Dbl
    this%R(2,2) = this%Lambda%U(2)

    ! Computing the eigenvectors inverse
    this%L(1,1) = this%Lambda%U(2)
    this%L(1,2) = -1.0_Dbl

    this%L(2,1) = -this%Lambda%U(1)
    this%L(2,2) = 1.0_Dbl

    c = this%Lambda%U(2) - this%Lambda%U(1)
    this%L(:,:)  =  this%L(:,:)   /(2.0_Dbl * c)

    ! Fill eigenvalue matrix
    this%Gam(1,1) = this%Lambda%U(1)
    this%Gam(1,2) = 0.0_Dbl
    this%Gam(2,1) = 0.0_Dbl
    this%Gam(2,2) = this%Lambda%U(2)

    ! Fill Gamma plus
    this%Gam_plus(1,1) = dmax1(this%Lambda%U(1), 0.0_Dbl)
    this%Gam_plus(1,2) = 0.0_Dbl
    this%Gam_plus(2,1) = 0.0_Dbl
    this%Gam_plus(2,2) = dmax1(this%Lambda%U(2), 0.0_Dbl)

    ! Fill Gamma minus
    this%Gam_minus(1,1) = dmin1(this%Lambda%U(1), 0.0_Dbl)
    this%Gam_minus(1,2) = 0.0_Dbl
    this%Gam_minus(2,1) = 0.0_Dbl
    this%Gam_minus(2,2) = dmin1(this%Lambda%U(2), 0.0_Dbl)


    ! Compute A plus
    this%A_plus  = matmul(matmul(this%R, this%Gam_plus), this%L)

    ! Compute A minus
    this%A_minus = matmul(matmul(this%R, this%Gam_minus), this%L)

    ! Compute A abs
    this%A_abs = this%A_plus - this%A_minus
  else
    print*, "Fatal error: the option of the Jacobian interpolation has not been defined."
    stop
  end if


!write(*,       *) " end subroutine < Jacobian_sub >"
!write(FileInfo,*) " end subroutine < Jacobian_sub >"
return
end subroutine Jacobian_sub


!##################################################################################################
! Purpose: This subroutine computes the eiqenvalues of a 2x2 matrix.
!
! Developed by: Babak Poursartip
! Supervised by: Clint Dawson
!
! The Institute for Computational Engineering and Sciences (ICES)
! The University of Texas at Austin
!
! ================================ V E R S I O N ==================================================
! V0.00: 03/19/2018 - Subroutine initiated.
! V0.01: 03/20/2018 - Initiated: Compiled without error for the first time.
!
! File version $Id $
!
! Last update: 03/20/2018
!
! ================================ L O C A L   V A R I A B L E S ==================================
! (Refer to the main code to see the list of imported variables)
!  . . . . . . . . . . . . . . . . Variables . . . . . . . . . . . . . . . . . . . . . . . . . . .
!
!##################################################################################################

subroutine Eigenvalues_sub(A, Lambda1, Lambda2)


! Libraries =======================================================================================

! User defined modules ============================================================================

implicit none

! Global variables ================================================================================

! - integer variables -----------------------------------------------------------------------------

! - real variables --------------------------------------------------------------------------------
real(kind=Dbl),     intent(out)    :: Lambda1, Lambda2

! - complex variables -----------------------------------------------------------------------------

! - integer Arrays --------------------------------------------------------------------------------

! - real Arrays -----------------------------------------------------------------------------------
real(kind=Dbl), intent(in), dimension (:,:)  :: A

! - character variables ---------------------------------------------------------------------------
! - logical variables -----------------------------------------------------------------------------

! Local variables =================================================================================
! - integer variables -----------------------------------------------------------------------------
! - real variables --------------------------------------------------------------------------------
real(kind=Dbl)      :: a11, a12, a21, a22

! code ============================================================================================
!write(*,       *) " subroutine < Eigenvalues_sub >: "
!write(FileInfo,*) " subroutine < Eigenvalues_sub >: "

a11 = A (1,1)
a12 = A (1,2)
a21 = A (2,1)
a22 = A (2,2)

Lambda1 = ((a11+a22) + dsqrt((a11+a22)**2 - 4.0_Dbl*(a11*a22-a12*a21)))/2.0_Dbl
Lambda2 = ((a11+a22) - dsqrt((a11+a22)**2 - 4.0_Dbl*(a11*a22-a12*a21)))/2.0_Dbl

!write(*,       *) " end subroutine < Eigenvalues_sub >"
!write(FileInfo,*) " end subroutine < Eigenvalues_sub >"
return
end subroutine Eigenvalues_sub


!##################################################################################################
! Purpose: This subroutine computes the inverse of a 2x2 matrix.
!
! Developed by: Babak Poursartip
! Supervised by: Clint Dawson
!
! The Institute for Computational Engineering and Sciences (ICES)
! The University of Texas at Austin
!
! ================================ V E R S I O N ==================================================
! V0.00: 03/26/2018 - Subroutine initiated.
! V0.01: 03/26/2018 - Initiated: Compiled without error for the first time.
!
! File version $Id $
!
! Last update: 03/26/2018
!
! ================================ L O C A L   V A R I A B L E S ==================================
! (Refer to the main code to see the list of imported variables)
!  . . . . . . . . . . . . . . . . Variables . . . . . . . . . . . . . . . . . . . . . . . . . . .
!
!##################################################################################################

subroutine Inverse(Matrix_in, Matrix_out)


! Libraries =======================================================================================

! User defined modules ============================================================================


implicit none

! Global variables ================================================================================

! - integer variables -----------------------------------------------------------------------------
! - real variables --------------------------------------------------------------------------------
! - complex variables -----------------------------------------------------------------------------
! - integer Arrays --------------------------------------------------------------------------------
! - real Arrays -----------------------------------------------------------------------------------
real(kind=Dbl),  intent(in),  dimension (:,:)  :: Matrix_in
real(kind=Dbl),  intent(out), dimension (:,:)  :: Matrix_out


! Local variables =================================================================================
! - integer variables -----------------------------------------------------------------------------
! - real variables --------------------------------------------------------------------------------
real(kind=Dbl) :: determinant
! - real Arrays -----------------------------------------------------------------------------------

! code ============================================================================================
!write(*,       *) " subroutine < Inverse >: "
!write(FileInfo,*) " subroutine < Inverse >: "

! determinant
determinant = Matrix_in(1,1) * Matrix_in(2,2) - Matrix_in(1,2) * Matrix_in(2,1)


Matrix_out(1,1) = +Matrix_in(2,2)
Matrix_out(1,2) = -Matrix_in(1,2)
Matrix_out(2,1) = -Matrix_in(2,1)
Matrix_out(2,2) = +Matrix_in(1,1)

Matrix_out(:,:) = Matrix_out(:,:) / determinant


!write(*,       *) " end subroutine < Inverse >"
!write(FileInfo,*) " end subroutine < Inverse >"
return
end subroutine Inverse


end module LaxWendroff_with_limiter_mod
