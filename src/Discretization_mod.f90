
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
! Last update: 02/26/2018
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
    module procedure Discretize_1D
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
! Last update: 02/26/2018
!
! ================================ L O C A L   V A R I A B L E S ==================================
! (Refer to the main code to see the list of imported variables)
!  . . . . . . . . . . . . . . . . variables . . . . . . . . . . . . . . . . . . . . . . . . . . .
!
!##################################################################################################

subroutine Discretize_1D(                                             &
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
type(Geometry_tp), intent(in) :: Geometry
type(InitialData_tp), intent(in) :: InitialInfo
type(discretization_tp), intent(out) :: Discretization

! Local variables =================================================================================
! - integer variables -----------------------------------------------------------------------------
integer (kind=Lng) :: i_reach

! - real variables --------------------------------------------------------------------------------
!#real (kind=Dbl)      ::
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

! code ============================================================================================
write(*,       *) " subroutine <Discretize_1D>: "
write(FileInfo,*) " subroutine <Discretize_1D>: "

write(*,       *) " -Discretizing the domain ..."
write(FileInfo,*) " -Discretizing the domain ..."




! Find total number of cells in the domain:
Discretization%NCells = 0_Lng

write(*,fmt="(A)") " Calculating the total number of the cells in the domain ... "

forall (i_reach = 1_Lng, InitialInfo%NoReaches) Discretization%NCells = Discretization%NCells + Geometry%ReachDisc(i_reach)
write(*,fmt="(A,I15)") " Total number of cells: ", Discretization%NCells

allocate( ,  ,  ,     stat=ERR_Alloc)

  if (ERR_Alloc /= 0) then
    write (*, Fmt_ALLCT) ERR_Alloc;  write (UnInf, Fmt_ALLCT) ERR_Alloc;
    write(*, Fmt_FL);  write(UnInf, Fmt_FL); read(*, Fmt_End);  stop;
  end if




        self.Length_Cell  = np.zeros(self.N_Cells, dtype=np.float) # Stores the length of each cell
        self.S_Cell       = np.zeros(self.N_Cells, dtype=np.float) # Stores bottom elevation of each cell
        self.Z_Cell       = np.zeros(self.N_Cells, dtype=np.float) # Stores bottom elevation of each cell
        self.Z_Full       = np.zeros(self.N_Cells*2+1, dtype=np.float) # Stores bottom elevation of each cell
        self.Manning_Cell = np.zeros(self.N_Cells, dtype=np.float) # Stores the Manning's number of each cell
        self.Width_Cell   = np.zeros(self.N_Cells, dtype=np.float) # Stores the Manning's number of each cell
        self.X_Disc       = np.zeros(self.N_Cells, dtype=np.float) # Stores the Manning's number of each cell
        self.X_Full       = np.zeros(self.N_Cells*2+1, dtype=np.float) # Stores the Manning's number of each cell



write(*,fmt="(A)")" Loop over reaches to discretize the domain ..."

        # Finding the highest point in the domain:
        print(" Calculating the highest point in the domain ... ")
        Max_Height = 0
        for ii in range(Temp_No_reaches):
            Max_Height += Experiment.Reach_Slope[ii] * Experiment.Reach_Length[ii]
        print("{} {:f}".format(" Maximum height is:", Max_Height))

        print(" Basic calculations ...")
        Cell_Counter = 0
        for ii in range(Temp_No_reaches):
            if Experiment.Reach_Type[ii]==0:
                CntrlVolume_Length = round(Experiment.Reach_Length[ii]/Experiment.Reach_Disc[ii],10)  # Control volume length, rounded to 5 decimal points. The length of the final cell in reach would be adjusted to fix the discretization, if necessary.
                print("   Cell length in the reach %d is: %f" % (ii+1, CntrlVolume_Length))
                Height = Max_Height
                Z_loss = round(CntrlVolume_Length * Experiment.Reach_Slope[ii],10)
                Height += round(0.5 * Z_loss,10)
                Total_Length = 0

                X_distance = 0.5 * CntrlVolume_Length
                for jj in range(ii):
                    X_distance  += Experiment.Reach_Length[jj]

                for jj in range( Experiment.Reach_Disc[ii] - 1 ):
                    self.Length_Cell[Cell_Counter]  = CntrlVolume_Length
                    self.X_Disc[Cell_Counter]       = X_distance
                    self.X_Full[Cell_Counter*2]     = X_distance - 0.5 * CntrlVolume_Length
                    self.X_Full[Cell_Counter*2+1]   = X_distance
                    X_distance                     += CntrlVolume_Length
                    Total_Length                   += CntrlVolume_Length
                    Height                         -= Z_loss

                    self.S_Cell[Cell_Counter]       = Experiment.Reach_Slope[ii]
                    self.Z_Cell[Cell_Counter]       = Height
                    self.Z_Full[Cell_Counter*2]     = Height +0.5 * Z_loss
                    self.Z_Full[Cell_Counter*2+1]   = Height
                    self.Manning_Cell[Cell_Counter] = Experiment.Reach_Manning[ii]
                    self.Width_Cell[Cell_Counter]   = Experiment.Reach_Width[ii]
                    Cell_Counter += 1

                # The last cell: we need to separate the last cell in each reach to adjust the numerical error in the total length of the reach
                self.Length_Cell[Cell_Counter]  = Experiment.Reach_Length[ii] - Total_Length
                X_distance                      = X_distance - 0.5 * CntrlVolume_Length + 0.5 * self.Length_Cell[Cell_Counter]
                self.X_Disc[Cell_Counter]       = X_distance
                self.X_Full[Cell_Counter*2]     = X_distance - 0.5 * self.Length_Cell[Cell_Counter]
                self.X_Full[Cell_Counter*2+1]   = X_distance
                self.X_Full[Cell_Counter*2+2]   = X_distance + 0.5 * self.Length_Cell[Cell_Counter]
                Height                         -= ( 0.5*Z_loss + 0.5 * self.Length_Cell[Cell_Counter] * Experiment.Reach_Slope[ii] )

                self.Z_Cell[Cell_Counter]       = Height
                self.Z_Full[Cell_Counter*2]     = Height + 0.5 * self.Length_Cell[Cell_Counter] * Experiment.Reach_Slope[ii]
                self.Z_Full[Cell_Counter*2+1]   = Height
                self.Z_Full[Cell_Counter*2+2]   = Height - 0.5 * self.Length_Cell[Cell_Counter] * Experiment.Reach_Slope[ii]
                self.Manning_Cell[Cell_Counter] = Experiment.Reach_Manning[ii]
                self.Width_Cell[Cell_Counter]   = Experiment.Reach_Width[ii]
                Cell_Counter += 1
                Max_Height   -= Experiment.Reach_Length[ii] * Experiment.Reach_Slope[ii]

            elif Experiment.Reach_Type[ii]==1:
                Height = Max_Height
                Projection_Length = round(Experiment.Reach_Length[ii]/Experiment.Reach_Disc[ii],10)
                #print(Projection_Length)
                X_distance = 0.5 * Projection_Length

                for jj in range(ii):
                    #print("X-distance: ",X_distance) # <delete>
                    X_distance  += Experiment.Reach_Length[jj]

                for jj in range( Experiment.Reach_Disc[ii] ):
                    Z_loss = Func (X_distance)
                    #print(X_distance,Z_loss) # <delete>

                    self.Length_Cell[Cell_Counter] = (Projection_Length**2 + Z_loss**2)**0.5
                    self.X_Disc[Cell_Counter] = X_distance
                    self.X_Full[Cell_Counter*2] = X_distance - 0.5 * Projection_Length
                    self.X_Full[Cell_Counter*2+1] = X_distance

                    self.S_Cell[Cell_Counter]       = DFunc(X_distance)
                    self.Z_Cell[Cell_Counter]       = Height + Z_loss
                    self.Z_Full[Cell_Counter*2]     = Height + Func(X_distance - 0.5 * Projection_Length)
                    self.Z_Full[Cell_Counter*2+1]   = Height + Z_loss
                    self.Manning_Cell[Cell_Counter] = Experiment.Reach_Manning[ii]
                    self.Width_Cell[Cell_Counter]   = Experiment.Reach_Width[ii]
                    X_distance += Projection_Length
                    Total_Length += Projection_Length
                    Cell_Counter += 1

                Max_Height   -= Experiment.Reach_Length[ii] * Experiment.Reach_Slope[ii]

        self.Q_Up       = Experiment.Q_Up
        self.V_in       = Experiment.V_in
        self.V_ratio    = Experiment.V_ratio
        self.Total_Time = Experiment.Total_Time
        self.Time_Step  = Experiment.Time_Step
        self.h_dw       = Experiment.h_dw

        if self.N_Cells != Cell_Counter:
            sys.exit("FATAL ERROR: Mismatch between the number of cells! Check the Discretization_Class.")

        del Experiment

        # Plot the discretized domain
        # Title = "Discretized domain at the cell level"
        # Vis =Draw.Plot_Domain(self.N_Cells, self.X_Disc, self.Z_Cell, Title)
        # Vis =Draw.Plot_Domain(2*self.N_Cells+1, self.X_Full, self.Z_Full, Title)

        print(" Discretization ends successfully. ")
        print(" ========== Discretization Class ==========")
        print()









write(*,       *) " -Domain discretized successfully."
write(FileInfo,*) " -Domain discretized successfully."

write(*,       *) " end subroutine <Discretize_1D>"
write(FileInfo,*) " end subroutine <Discretize_1D>"
return
end subroutine Discretize_1D



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

real(kind=Dbl), intent(in)  :: x
real(kind=Dbl), intent(out) :: Bathymetry


! code ============================================================================================

Bathymetry = lambda x: 0.2 - 0.05 * (x-10)**2

end function Domain_Func_1D

function Domain_Func_1D_D(x) result(DBathymetry)

implicit none

real(kind=Dbl), intent(in)  :: x
real(kind=Dbl), intent(out) :: DBathymetry


! code ============================================================================================

DBathymetry = lambda x: - 0.05 * 2 * (x-10)

end function Domain_Func_1D




end module Discretization_mod
