
! Integer Variables -------------------------------------------------------------------------------

! Tiny Integers
Integer (Kind=Tiny)  :: ;                   ! ---


! Smll Integers
Integer (Kind=Smll)  :: Year, Month, Day ;               ! Date variables
Integer (Kind=Smll)  :: Hour, Minute, Seconds, S100th ;  ! Time variables
Integer (Kind=Smll)  :: IO_File ;                ! For IOSTAT: Input Output Status in OPEN cammand
Integer (Kind=Smll)  :: ERR_Alloc, ERR_DeAlloc ; ! Allocating and DeAllocating errors
Integer (Kind=Smll)  :: UnFile ;                 ! Holds Unit of a file for error message

! Shrt Integers
Integer (Kind=Shrt)  :: ;                        ! 

! Lng Integers
Integer (Kind=Lng )  :: ;                        ! 


! - Real Variables ----------------------------------------------------------------------------------------------------------------------------------
Real (Kind=DBL)      :: TimeS, TimeE ;           ! TIME Variables for total runtime
Real (Kind=DBL)      :: TimeInputS, TimeInputE ; ! TIME Variables for reading input files
Real (Kind=DBL)      :: TimeAssemS, TimeAssemE ; ! TIME Variables for forming global matrices
Real (Kind=DBL)      :: TimeIndexS, TimeIndexE ; ! TIME Variables for object definition and Indexing
Real (Kind=DBL)      :: TimeEStiffS, TimeEStiffE;! TIME Variables for Effective Stiffness
Real (Kind=DBL)      :: TimeSolveS, TimeSolveE ; ! TIME Variables for Effective Stiffness

! - Logical Variable --------------------------------------------------------------------------------------------------------------------------------
Logical (Kind=Shrt)  :: Directory ;

! - Integer Arrays ----------------------------------------------------------------------------------------------------------------------------------
! - Real Arrays -------------------------------------------------------------------------------------------------------------------------------------

! - Integer ARRAY ALLOCATION ------------------------------------------------------------------------------------------------------------------------
! Tiny Integers
Integer (Kind=Tiny),              Dimension(6)   :: LoadC ;          ! Load Case, Indicates which kinds of static load should be consider in analysis (1 2 3 4) - 1: body force - 2: pressure - 3: concentrated force - 4: specified Displacements - 5: Static/Dynaimc analysis/both: 0/1/2

! Smll Integers
Integer (Kind=Smll), Allocatable, Dimension(:)   :: LTEL ;           ! Load Type of each Element


! Shrt Integers
Integer (Kind=Shrt), Allocatable, Dimension(:)   :: MTEL ;           ! Material Type of each Element
Integer (Kind=Shrt), Allocatable, Dimension(:)   :: ELT ;            ! Element Type of each Element
Integer (Kind=Shrt), Allocatable, Dimension(:)   :: STEP ;           ! Holds steps in which full information in dynamic analysis is required.
Integer (Kind=Shrt), Allocatable, Dimension(:)   :: ELGR ;           ! Holds Group number of each element.
Integer (Kind=Shrt), Allocatable, Dimension(:)   :: ND_b ;           ! Holds the equation number of DRM boundary nodes. This array cannot be a LNG, because it has a conflict with PETSc lib.
Integer (Kind=Shrt), Allocatable, Dimension(:)   :: ND_e ;           ! Holds the equation number of DRM layer nodes
Integer (Kind=Shrt), Allocatable, Dimension(:)   :: ND_DC ;          ! Holds the equation number of nodes of the element that has the Double couple location

! Lng Integers
Integer (Kind=Lng ), Allocatable, Dimension(:,:) :: NEG ;            ! Number of Elements of each Group
Integer (Kind=Lng ), Allocatable, Dimension(:)   :: JLoad ;          ! Joint Load, Holds node numbers with concentrated force.
Integer (Kind=Lng ), Allocatable, Dimension(:)   :: NDAN ;           ! Holds Node numbers in which history of Displacements are required based on Application Numbering
Integer (Kind=Lng ), Allocatable, Dimension(:)   :: NVAN ;           ! Holds Node numbers in which history of Velocity are required based on Application Numbering
Integer (Kind=Lng ), Allocatable, Dimension(:)   :: NAAN ;           ! Holds Node numbers in which history of Acceleration are required based on Application Numbering
Integer (Kind=Lng ), Allocatable, Dimension(:)   :: NoBndry_DRM ;    ! Holds Node numbers on the DRM boundary
Integer (Kind=Lng ), Allocatable, Dimension(:)   :: NoLayer_DRM ;    ! Holds Node numbers on the DRM layer
Integer (Kind=Lng ), Allocatable, Dimension(:)   :: DoubleCouple_El; ! Holds Node numbers on the DRM layer
Integer (Kind=Lng ), Allocatable, Dimension(:,:) :: INOD ;           ! Node Connectivity.
Integer (Kind=Lng ), Allocatable, Dimension(:,:) :: ID ;             ! Identification matrix - Global constrains of the nodes.
Integer (Kind=Lng ), Allocatable, Dimension(:,:) :: IDBC ;           ! Identification of Boundary Condition


! - Real ARRAY ALLOCATION ---------------------------------------------------------------------------------------------------------------------------
Real (Kind=DBL)   , Allocatable, Dimension(:)    :: F ;
Real (Kind=DBL)   , Allocatable, Dimension(:)    :: K ;              ! Global Stiffness Matrix saved in compact form - either Skyline or CSR
Real (Kind=DBL)   , Allocatable, Dimension(:)    :: M ;              ! Global Mass Matrix saved in compact form - either Skyline or CSR
Real (Kind=DBL)   , Allocatable, Dimension(:)    :: VM ;             ! Global Diagonal Mass Matrix in Explicit method
Real (Kind=DBL)   , Allocatable, Dimension(:)    :: C ;              ! Global Damping Matrix saved in compact form - either Skyline or CSR
Real (Kind=DBL)   , Allocatable, Dimension(:)    :: InciWave ;       ! Properties of the incident wave for DRM. i.e. 1: theta, 2: omega 3: amplitude(U)

Real (Kind=DBL)   , Allocatable, Dimension(:,:)  :: PMat ;           ! Properties of MATerials.
Real (Kind=DBL)   , Allocatable, Dimension(:,:)  :: PBLD ;           ! Properties of the Body force LoaD.
Real (Kind=DBL)   , Allocatable, Dimension(:,:)  :: XYZ ;            ! Node Coordinates.
Real (Kind=DBL)   , Allocatable, Dimension(:,:)  :: UDis ;           ! Holds predefined Displacements in nodes (like settlement).
Real (Kind=DBL)   , Allocatable, Dimension(:,:)  :: PLoad ;          ! Holds concentrated forces on nodes (See JLoad).
Real (Kind=DBL)   , Allocatable, Dimension(:,:)  :: BACL ;           ! Base Acceleration.
Real (Kind=DBL)   , Allocatable, Dimension(:,:)  :: PML_DIM ;        ! PML Dimensions.

Real (Kind=DBL)   , Allocatable, Dimension(:,:)  :: P ;              ! Loads
Real (Kind=DBL)   , Allocatable, Dimension(:,:)  :: FK ;             ! Full Global stiffness Matrix
Real (Kind=DBL)   , Allocatable, Dimension(:,:)  :: FM ;             ! Full Global Mass Matrix
Real (Kind=DBL)   , Allocatable, Dimension(:,:)  :: FC ;             ! Full Global Damping Matrix
Real (Kind=DBL)   , Allocatable, Dimension(:,:)  :: M_eb ;           ! Holds the parts of the Mass matrix in the DRM layer for DRM 
Real (Kind=DBL)   , Allocatable, Dimension(:,:)  :: C_eb ;           ! Holds the parts of the damping matrix in the DRM layer for DRM 
Real (Kind=DBL)   , Allocatable, Dimension(:,:)  :: K_eb ;           ! Holds the parts of the stiffness matrix in the DRM layer for DRM

!Real (Kind=DBL)   , Allocatable, Dimension(:,:)  :: FKK ;   ! Full Global stiffness Matrix used in calculating the total energy of the regular domain
!Real (Kind=DBL)   , Allocatable, Dimension(:,:)  :: FMM ;   ! Full Global mass Matrix used in calculating the total energy of the regular domain    !?? delete after debug  


! - Type ALLOCATION ---------------------------------------------------------------------------------------------------------------------------------
Type ( PML_Interface ) :: PML_INTER ;            ! STIFFNESS AND MASS OF PML ELEMENTS AT THE Interface NODES    !?? delete this and move it the call subroutine @@@@@@@@@@@@@@
Type ( BasicParam )    :: Param ;                ! Holds basic parameters of each load case

! - Character Variables -----------------------------------------------------------------------------------------------------------------------------
Character (Kind = 1, Len = 30 ) :: ModelName ;   ! name of the model input file
Character (Kind = 1, Len = 30 ) :: AnaName ;     ! Name of the analysis input file
Character (Kind = 1, Len = 150) :: Model_InDir ;! Directory of Model input file.
Character (Kind = 1, Len = 150) :: Ana_InDir ;   ! Directory of Analysis input file.
Character (Kind = 1, Len = 150) :: InlDir ;      ! Directory of internal files.
Character (Kind = 1, Len = 150) :: InlDirAna ;   ! Directory of internal files for each analysis
Character (Kind = 1, Len = 150) :: OutDir ;      ! Directory of output files (Results)
Character (Kind = 1, Len = 150) :: OutDirAna ;   ! Directory of output files (Results) for each analysis

! =========================== Deleted Variables =====================================================================================================
!Integer (Kind=Tiny)  :: PR_Type ;                ! Pressure Type, 1: constant pressure on the element - 2: Linear pressure on the element
!Integer (Kind=Tiny)  :: LDIR ;                   ! Load Direction, Indicated the direction of the pressure on the element.
!Integer (Kind=Tiny)  :: PRZERO ;                 ! Pressure ???  1: pressure cannot be negative - 2: pressure can take any value
!Integer (Kind=Tiny)  :: NNode ;                  ! Number of Nodes of element
!Integer (Kind=Tiny)  :: NPlane ;                 ! 1: Plane Stress  2: Plane Strain 3: Not applicable, for 3D models
!Integer (Kind=Tiny)  :: NDamp ;                  ! Indicates whether there exists damping in the analysis. 0: No damping - 1: damping exists.
!Integer (Kind=Tiny)  :: NEnergy ;                ! Indicates whether total energy of the regular domain should be evaluated - 0: yes , 1:No
!Integer (Kind=Tiny)  :: Type_BC ;                ! Type of Boundary Condition,  1: SOMMERFELD - 2: SHARAN
!Integer (Kind=Tiny)  :: LOAD_Type ;              ! Load Type, 0: Ricker pulse   1: Load is base acceleraion
!Integer (Kind=Tiny)  :: FUNC_Type ;              ! Function Type, determines type of load. 1: Sine load 2: Ricker pulse
!Integer (Kind=Tiny)  :: PARAM_Type ;             ! Prameter Type determines how PML parameters are defined, 0 : READS RAW DATA TO FORM ALFA_0 AND BETA_0 - 1 : READS ALFA_0 AND BETA_0 DIRECTLY
!Integer (Kind=Tiny)  :: AnsysInput ;             ! Indicates if the mesh generator is Ansys, so that the node numbering of the second order elemenets is based of the ansys format
!Integer (Kind=Tiny)  :: AnsysOutput ;            ! Ansys output. 0: Generates nothin - 1: Generates a .log file as an Input for Ansys.
!Integer (Kind=Tiny)  :: Wave_Type ;              ! Used in DRM analysis. Determines the type of the incident wave. 0: SV-Wave  1: P-Wave
!
!Integer (Kind=Smll)  :: El_Type ;                ! Number of Analysis !????
!Integer (Kind=Smll)  :: NPBL ;                   ! Number of properties of body force load
!Integer (Kind=Smll)  :: NBLD ;                   ! Number of body force loads
!Integer (Kind=Smll)  :: NEqEl ;                  ! Number of equations of element
!Integer (Kind=Smll)  :: NEqElS ;                 ! Number of equations of element Solid degrees of freedom
!Integer (Kind=Smll)  :: NBC ;                    ! Number of Boundary Conditions.
!Integer (Kind=Smll)  :: InterfaceNeighbor ;      ! Maximum number of node neighbors at the interface of PML and regular domain
!
!Integer (Kind=Shrt)  :: STStep ;                 ! Number Steps for applying STatic load
!Integer (Kind=Shrt)  :: NLN ;                    ! Number of Loaded Nodes (Concentrated forces on nodes).
!Integer (Kind=Shrt)  :: NSND ;                   ! Number of nodes (Supports) with predefined Displacements.
!Integer (Kind=Shrt)  :: NEl_DRM ;                ! Number of elements on the boundary for Domain Reduction Method
!Integer (Kind=Shrt)  :: NNBndry_DRM ;            ! Number of nodes on the DRM boundary for the Domain Reduction Method
!Integer (Kind=Shrt)  :: NNLayer_DRM ;            ! Number of nodes on the DRM layer for the Domain Reduction Method
!Integer (Kind=Shrt)  :: NSTEP ;                  ! Total Number of steps in dynamic analysis.
!Integer (Kind=Shrt)  :: NTST ;                   ! Number of Steps (Times) which full information is required
!Integer (Kind=Shrt)  :: NASTEP ;                 ! Number of Steps of base acceleration in input file.
!Integer (Kind=Shrt)  :: NNDH ;                   ! Number of Nodes which History of Displacement is required in dynamic analysis.
!Integer (Kind=Shrt)  :: NNVH ;                   ! Number of Nodes which History of Velocity is required in dynamic analysis.
!Integer (Kind=Shrt)  :: NNAH ;                   ! Number of Nodes which History of Acceleration is required in dynamic analysis.
!Integer (Kind=Shrt)  :: NNT ;                    !?????????????
!
!Integer (Kind=Lng )  :: NJTotal ;                ! Total Number of Joints (Nodes) of model
!Integer (Kind=Lng )  :: NEQS ;                   ! Number of equations of the solid elements of the model 
!Integer (Kind=Lng )  :: NJInterface ;            ! Total number of Joints (nodes) on the interface of PML and regular domain
!
!
!Real (Kind=DBL)      :: G ;                      ! Gravity
!Real (Kind=DBL)      :: HREF ;                   ! Refrence height, this determines the height of water in dam reservoir. above this pressure is zero and below it is nonzero
!Real (Kind=DBL)      :: PR ;                     ! Pressure. actually, this is the density of fluid.
!Real (Kind=DBL)      :: ALFA, BETTA ;            ! Damping coefficients, Rayleigh damping method.          !?? see where we need them
!Real (Kind=DBL)      :: DELTA, GAMA ;            ! Newmark Coefficients.
!Real (Kind=DBL)      :: DT ;                     ! Time Duration for dynamic analysis
!Real (Kind=DBL)      :: t0 ;                     ! start time for the dynamic analysis
!
!Integer (Kind=Shrt), Allocatable, Dimension(:)   :: JD ;             ! @@@@@@@@@@@@@@@@@@@@@@@@@
!Integer (Kind=Shrt), Allocatable, Dimension(:)   :: NTK ;            ! @@@@@@@@@@@@@@@@@@@@@@
!
!
!Integer (Kind=Lng ), Allocatable, Dimension(:,:) :: El_DRM ;         ! Holds element numbers and face numbers on the boundary in DRM
!
!
!
!Real (Kind=DBL)   , Allocatable, Dimension(:)   :: EIGV ;
!Real (Kind=DBL)   , Allocatable, Dimension(:)   :: AG ;
!
!
!Real (Kind=DBL)   , Allocatable, Dimension(:,:)  :: M_bb ;           ! Holds the parts of the Mass matrix in the DRM layer for DRM 
!Real (Kind=DBL)   , Allocatable, Dimension(:,:)  :: C_bb ;           ! Holds the parts of the damping matrix in the DRM layer for DRM 
!Real (Kind=DBL)   , Allocatable, Dimension(:,:)  :: K_bb ;           ! Holds the parts of the stiffness matrix in the DRM layer for DRM


!Real (Kind=DBL)   , Allocatable, Dimension(:,:,:)         :: SIGMA_LI, SIGNA_DC, TENIntSTR, COMINTSTR ;
!Real (Kind=DBL)   , Allocatable, Dimension(:,:,:,:)       :: EPSL_DC, EQ_PL_STN, SIG_EFF ;
!Real (Kind=DBL)   , Allocatable, Dimension(:,:,:,:,:)     :: SIGMA ;
!Real (Kind=DBL) , Allocatable, Dimension(:,:,:,:,:,:)   ::  ;
!Real (Kind=DBL) , Allocatable, Dimension(:,:,:,:,:,:,:) ::  ;