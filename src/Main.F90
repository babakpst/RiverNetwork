
!##################################################################################################
! Purpose: This code solves the 2D Shallow Water Equation
!
! Developed by: Babak Poursartip
! Supervised by: Clint Dawson
! 
! The Institute for Computational Engineering and Sciences (ICES)
! The University of Texas at Austin
!
! ================================ V E R S I O N ==================================================
! V0.0: 08/02/2018  - Initiation.
!
! File version $Id
!
! Last update: 08/02/2018
!
! ================================ Global   V A R I A B L E S =====================================
!  . . . . . . . . . . . . . . . . Variables . . . . . . . . . . . . . . . . . . . . . . . . . . . 
!
!##################################################################################################


program SVE;


! Libraries =======================================================================================

! Defined Modules =================================================================================

Implicit None ;

! Global Variables ================================================================================
Include 'Global_Variables_Inc.F95'   ! All Global Variables are defined/described in this file

! Time and Date signature =========================================================================
Call CPU_TIME(Time_Start ) ;
Call GETDAT(Year, Month, Day) ;
Call GETTIM(Hour, Minute, Seconds, S100th) ;

! Write some information on screen 
Write(*,*)"<<<<<<<<<<<<<    Start analysis ...  >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>" ;


! Directories, input, and output files ============================================================
! Address file ------------------------------------------------------------------------------------
UnFile=File_Address ;
Open(Unit=UnFile, File='ADDRESS_PTC.TXT', Err=1001, IOSTAT=IO_File, ACCESS='SEQUENTIAL', &
     ACTION='READ', ASYNCHRONOUS='NO', BLANK='NULL', BLOCKSize=0, DisPOSE='KEEP', &
     FORM='FORMATTED', POSITION='ASIS', STATUS='OLD') ;

! Read the input fine name and directories form "ADDRESS_FILE.txt" in the current directory -------
Read(UN_ADR,*) ;
Read(UN_ADR,*) ModelName ;
Read(UN_ADR,*) ;
Read(UN_ADR,*) NAT, Output_Type ;
Read(UN_ADR,*) ;
Read(UN_ADR,*) Model_InDir ;
Read(UN_ADR,*) ;
Read(UN_ADR,*) InlDir ;
Read(UN_ADR,*) ;
Read(UN_ADR,*) OutDir ;
Read(UN_ADR,*) ;
Read(UN_ADR,*) AnaName ;
Read(UN_ADR,*) NumberOfAnalyses ;

Ana_InDir  =TRIM(AdjustL (Model_InDir))//'/'//TRIM(AdjustL (ModelName))//'/'//'Analysis' ;
Model_InDir=TRIM(AdjustL (Model_InDir))//'/'//TRIM(AdjustL (ModelName))//'/'//'Model' ;

Write (*,*)"Model_InDir", Model_InDir ;
Write (*,*)"Ana_InDir", Ana_InDir ;

! Model Data file ---------------------------------------------------------------------------------
UnFile=File_Input_Model ;
Open (Unit=UnFile, FILE=TRIM(ModelName)//'_'//Trim(AdjustL(IndexSize))//'_'//Trim(AdjustL(IndexRank))//'.dataModel', & 
      ERR=1001, IOSTAT=IO_File, ACCESS='SEQUENTIAL', ACTION='READ', ASYNCHRONOUS='NO', &
      BLANK='NULL', BLOCKSize=0, DEFAULTFILE=TRIM(Model_InDir), DisPOSE='KEEP', FORM='Formatted', & 
      POSITION='ASIS', STATUS='Old' ) ;

! Create the results folder -----------------------------------------------------------------------
Directory=MakeDirQQ (TRIM(AdjustL (OutDir))//'/'//TRIM(AdjustL (ModelName))  ) ;
  IF (Directory) THEN ;
     WRITE (*     ,*) 'New subdirectory successfully created - results folder' ;
  ELSE ;
     WRITE (*     ,*) 'Subdirectory is already exits - results folder' ;
  END IF ;

! Internal folder
Directory=MakeDirQQ (TRIM(AdjustL (InlDir))//'/'//TRIM(AdjustL (ModelName))) ;
  IF (Directory) THEN ;
     WRITE (*    ,*) 'New subdirectory successfully created - internal folder' ;
  ELSE ;
     WRITE (*     ,*) 'Subdirectory is already exits - results folder' ;
  END IF ;

OutDir=TRIM(AdjustL (OutDir))//'/'//TRIM(AdjustL (ModelName)) ;
InlDir=TRIM(AdjustL (InlDir))//'/'//TRIM(AdjustL (ModelName)) ;

Write (*,*)"OutDir", OutDir ;
Write (*,*)"InlDir", InlDir ;

! Opening the information file --------------------------------------------------------------------
UnFile=File_Info ;
Open (Unit=UnFile, FILE=TRIM(ModelName)//'_'//Trim(AdjustL(IndexSize))//'_'//Trim(AdjustL(IndexRank))//'.infM', & 
      ERR=1001, IOSTAT=IO_File, ACCESS='SEQUENTIAL', ACTION='Write', ASYNCHRONOUS='NO', & 
      BLANK='NULL', BLOCKSize=0, DEFAULTFILE=TRIM(OutDir), DisPOSE='KEEP', FORM='FORMATTED', &
      POSITION='ASIS', STATUS='REPLACE' ) ;
















      ! Writing down the time signature =================================================================
Call INFO ( Iyr, Imon, Iday, Ih, Im, Is, I100th,     ModelName, Ana_InDir, OutDir, InlDir ) ;

! Reading model data ==============================================================================

! Reading Input FILE ------------------------------------------------------------------------------
Call CPU_TIME( TimeInputS ) ;

! Reading basic data
  If ( Output_Type == 0 .OR. Output_Type == 2 ) Then ;  ! Ordinary files OR HDF5 files - Reads data from an ordinary format based on PTC_Input sub:Input_BASIC - This input generates by PIC.
    Call Input (                                                                                                                    &
    NDOF, MaxNNode, NDim,                                                                                                           & ! Integer (1) Variables
    NGroup, NPM, NMat,                                                                                                              & ! Integer (2) Variables
    NEL, NJ, NJTotal, NEQM, NEQMTotal, NEQM_Mapping,                                                                                & ! Integer (4) Variables
    !                                                                                                                               & ! Integer (8) Variables
    LoadC,                                                                                                                          & ! Real Variables
    !                                                                                                                               & ! Integer Arrays
    !                                                                                                                               & ! Real Arrays
    !                                                                                                                               & ! Characters
    Param                                                                                                                           & ! Type
    ) ;
  Else If ( Output_Type == 1 ) Then ; ! Binary files
    Call Input_Binary (                                                                                                             &
    NDOF, MaxNNode, NDim,                                                                                                           & ! Integer (1) Variables
    NGroup, NPM, NMat,                                                                                                              & ! Integer (2) Variables
    NEL, NJ, NJTotal, NEQM, NEQMTotal, NEQM_Mapping,                                                                                & ! Integer (4) Variables
    !                                                                                                                               & ! Integer (8) Variables
    LoadC,                                                                                                                          & ! Real Variables
    !                                                                                                                               & ! Integer Arrays
    !                                                                                                                               & ! Real Arrays
    !                                                                                                                               & ! Characters
    Param                                                                                                                           & ! Type
    ) ;
  End If ;

! Allocating required arrays
Allocate ( ,
           STAT=ERR_Alloc) ;
  If ( ERR_Alloc /= 0 ) Then ;
    Write (*, Fmt_ALLCT) ERR_Alloc ;  Write (UnInf, Fmt_ALLCT) ERR_Alloc ;
    Write(*, Fmt_FL) ;  Write(UnInf, Fmt_FL) ; Write(*, Fmt_End) ; Read(*,*) ;  STOP ;
  End If ;

! Open require files
Include 'PTC_Open_Inc.F90'  ! Include file for opening files

! Reading input arrays
  If ( Output_Type == 0 ) Then ;
    Call Input (                                                                                                                    &
    NDOF, NDim, MaxNNode,                                                                                                           & ! Integer (1) Variables
    NGroup, NPM, NMat,     NLCase,                                                                                                  & ! Integer (2) Variables
    !                                                                                                                               & ! Integer (4) Variables
    NEL, NJ, NEQM, NEQM_Mapping,                                                                                                    & ! Integer (8) Variables
    !                                                                                                                               & ! Real Variables
    LoadC,     IDBC,      MTEL, ELT,     ELGR,                                                                                      &
    D_NNZ_Stiff, O_NNZ_Stiff, D_NNZ_Damp, O_NNZ_Damp, D_NNZ_Mass, O_NNZ_Mass,     App_Numbers, PETSc_Numbers,     Indices,          &
    NDAN , NVAN, NAAN, NDPN , NVPN, NAPN,     NoBndry_DRM, NoLayer_DRM, JLoad,     EqDis, EqVel, EqAcc,      INOD, ID,              & ! Integer Arrays
    PMat, PBLD, XYZ, UDis, PLoad, PML_DIM,                                                                                          & ! Real Arrays
    !                                                                                                                               & ! Characters
    Param                                                                                                                           & ! Type
    ) ;
  Else If ( Output_Type == 1 ) Then ;
    Call Input_Binary (                                                                                                             &
    NDOF, NDim, MaxNNode,                                                                                                           & ! Integer (1) Variables
    NGroup, NPM, NMat,     NLCase,                                                                                                  & ! Integer (2) Variables
    !                                                                                                                               & ! Integer (4) Variables
    NEL, NJ, NEQM, NEQM_Mapping,                                                                                                    & ! Integer (8) Variables
    !                                                                                                                               & ! Real Variables
    LoadC,     IDBC,      MTEL, ELT,     ELGR,                                                                                      &
    D_NNZ_Stiff, O_NNZ_Stiff, D_NNZ_Damp, O_NNZ_Damp, D_NNZ_Mass, O_NNZ_Mass,     App_Numbers, PETSc_Numbers,     Indices,          &
    NDAN , NVAN, NAAN, NDPN , NVPN, NAPN,     NoBndry_DRM, NoLayer_DRM, JLoad,     EqDis, EqVel, EqAcc,      INOD, ID,              & ! Integer Arrays
    PMat, PBLD, XYZ, UDis, PLoad, PML_DIM,                                                                                          & ! Real Arrays
    !                                                                                                                               & ! Characters
    Param                                                                                                                           & ! Type
    ) ;
  Else If ( Output_Type == 2 ) Then ;
    Call Input_HDF5 (                                                                                                               &
    NDOF, NDim, MaxNNode,                                                                                                           & ! Integer (1) Variables
    NGroup, NPM, NMat,     NLCase,                                                                                                  & ! Integer (2) Variables
    !                                                                                                                               & ! Integer (4) Variables
    NEL, NJ, NEQM, NEQM_Mapping,                                                                                                    & ! Integer (8) Variables
    !                                                                                                                               & ! Real Variables
    LoadC,     IDBC,      MTEL, ELT,     ELGR,                                                                                      &
!    D_NNZ_Stiff, O_NNZ_Stiff, D_NNZ_Damp, O_NNZ_Damp, D_NNZ_Mass, O_NNZ_Mass,     App_Numbers, PETSc_Numbers,     Indices,          &
    App_Numbers, PETSc_Numbers,     Indices,                                                                                        &
    NDAN , NVAN, NAAN, NDPN , NVPN, NAPN,     NoBndry_DRM, NoLayer_DRM, DoubleCouple_El, JLoad,     EqDis, EqVel, EqAcc,      INOD, ID,              & ! Integer Arrays
    PMat, PBLD, XYZ, UDis, PLoad, PML_DIM,                                                                                          & ! Real Arrays
    ModelName, IndexRank, IndexSize, Model_InDir,                                                                                   & ! Characters
    Param,                                                                                                                          & ! Type
    Rank &
    ) ;
  End If ;
Call CPU_TIME( TimeInputE ) ;

! =========================== Close FILES =========================================================
! - Close Input FILE ------------------------------------------------------------------------------

Include 'PTC_Close_Inc.F90' 


! Simulations =====================================================================================
!

Read(UN_ADR,*) ;
  Analysis_Loop: Do IAnalysis=1, NumberOfAnalyses ;   ! Loop over the number of analyses ------------------------
    Write (*,*)"Ananlysis Number: ", IAnalysis ;
    Read(UN_ADR,*) AnaName ;
    Write (*,*)"Ananlysis Name: ",AnaName ;

    ! Analysis Data file 
    UnFile=UnInptAna ;
    Open ( Unit=UnFile, FILE=TRIM(AnaName)//'.txt', ERR= 1001, IOSTAT=IO_File, ACCESS='SEQUENTIAL', ACTION='READ', ASYNCHRONOUS='NO', BLANK='NULL', BLOCKSize=0, DEFAULTFILE=TRIM(Ana_InDir), DisPOSE='KEEP', FORM='FORMATTED', POSITION='ASIS', STATUS='Old' ) ;

    Directory=MakeDirQQ (TRIM(AdjustL (OutDir))//'/'//TRIM(AdjustL (AnaName))  ) ;
      IF (Directory) THEN ;
         WRITE (*     ,*) 'New subdirectory successfully created' ;
         WRITE (UnInf,*) 'New subdirectory successfully created' ;
      ELSE ;
         WRITE (*    ,*) 'Subdirectory already exists' ;
         WRITE (UnInf,*) 'Subdirectory already exists' ;
      END IF ;


    Directory=MakeDirQQ (TRIM(AdjustL (InlDir))//'/'//TRIM(AdjustL (AnaName))  ) ;
      IF (Directory) THEN ;
         WRITE (*    ,*) 'New subdirectory successfully created  - Check folders' ;
         WRITE (UnInf,*) 'New subdirectory successfully created  - Check folders' ;
      ELSE ;
         WRITE (*    ,*) 'Subdirectory already exists' ;
         WRITE (UnInf,*) 'Subdirectory already exists' ;
      END IF ;

    OutDirAna=TRIM(AdjustL (OutDir))//'/'//TRIM(AdjustL (AnaName)) ;
    InlDirAna=TRIM(AdjustL (InlDir))//'/'//TRIM(AdjustL (AnaName)) ;

    ! - CHECK FILE --------------------------------------------------------------------------------------------------------------------------------------
    UnFile=UN_CHK ;
    Open ( Unit=UnFile, FILE=TRIM(AnaName)//'_'//Trim(AdjustL(IndexSize))//'_'//Trim(AdjustL(IndexRank))//'.Chk' , ERR= 1001, IOSTAT=IO_File, ACCESS='SEQUENTIAL', ACTION='Write', ASYNCHRONOUS='NO', BLANK='NULL', BLOCKSize=0, DEFAULTFILE=TRIM(InlDirAna), DisPOSE='KEEP', FORM='FORMATTED', POSITION='ASIS', STATUS='REPLACE' ) ;

    ! =============================================== Analysis ==========================================================================================
      SELECT CASE ( NAT ) ;

    ! ---------------------------------------------------------------------------------------------------------------------------------------------------
    ! - Analytical solution of wave propagation in half-space
    ! ---------------------------------------------------------------------------------------------------------------------------------------------------
        CASE ( ACN_Analytical_DRM ) ;    ! # 73

          Include 'PTC_Case_Anal_DRM.F90'

    ! ---------------------------------------------------------------------------------------------------------------------------------------------------
    ! -  Standard FEM  Implicit 2D
    ! ---------------------------------------------------------------------------------------------------------------------------------------------------
        CASE ( ACN_FEM_Implicit_2D ) ;    ! # 61

          Include 'PTC_Case_FEM_Imp_2D.F90'

    ! ---------------------------------------------------------------------------------------------------------------------------------------------------
    ! - Spectral Implicit 2D DRM
    ! ---------------------------------------------------------------------------------------------------------------------------------------------------
        CASE ( ACN_FEM_Implicit_2D_DRM ) ;    ! # 64

          Include 'PTC_Case_FEM_Imp_2D_DRM.F90'

    ! ---------------------------------------------------------------------------------------------------------------------------------------------------
    ! - Spectral Explicit 2D
    ! ---------------------------------------------------------------------------------------------------------------------------------------------------
        CASE ( ACN_Spec_Explicit_2D ) ;    ! # 63

          Include 'PTC_Case_Spc_Exp_2D.F90'

    ! ---------------------------------------------------------------------------------------------------------------------------------------------------
    ! - Spectral Explicit 3D - Use this case for both surface load and point load
    ! ---------------------------------------------------------------------------------------------------------------------------------------------------
        CASE ( ACN_Spec_Explicit_3D ) ;    ! # 69

          Include 'PTC_Case_Spc_Exp_3D.F90'

    ! ---------------------------------------------------------------------------------------------------------------------------------------------------
    ! - Spectral Explicit 2D DRM
    ! ---------------------------------------------------------------------------------------------------------------------------------------------------
        CASE ( ACN_Spec_Explicit_2D_DRM ) ;    ! # 66

          Include 'PTC_Case_Spc_Exp_2D_DRM.F90'

    ! ---------------------------------------------------------------------------------------------------------------------------------------------------
    ! - Spectral method Explicit 3D DRM
    ! ---------------------------------------------------------------------------------------------------------------------------------------------------
        CASE ( ACN_Spec_Explicit_3D_DRM ) ;    ! # 72

          Include 'PTC_Case_Spc_Exp_3D_DRM.F90'

    ! ---------------------------------------------------------------------------------------------------------------------------------------------------
    ! - Spectral method Explicit 3D Double Couple
    ! ---------------------------------------------------------------------------------------------------------------------------------------------------
        CASE ( ACN_Spec_Explicit_3D_DC ) ;    ! # 75

          Include 'PTC_Case_Spc_Exp_3D_DC.F90'

    ! ---------------------------------------------------------------------------------------------------------------------------------------------------
    ! - Spectral method Implicit 2D
    ! ---------------------------------------------------------------------------------------------------------------------------------------------------
        CASE ( ACN_Spec_Implicit_2D ) ;    ! # 62

          Include 'PTC_Case_Spc_Imp_2D.F90'

    ! ---------------------------------------------------------------------------------------------------------------------------------------------------
    ! - Spectral method Frequency domain solution   2D
    ! ---------------------------------------------------------------------------------------------------------------------------------------------------
!        CASE ( ACN_Spec_Freq_2D_DRM ) ;    ! # 74
!
!          Include 'PTC_Case_Spc_Freq.F90'

        ! - ERROR IN ANALYSIS NUMBERING ---------------------------------------------------------------------------------------------------------------------
        CASE DEFAULT ;
          Write(*,*)" Type OF ANALYSIS IS NOT AVAILABLE IN THE MAIN SELECT CASE - CHECK THE Input FILE" ;
          Write(UnInf,*)" Type OF ANALYSIS IS NOT AVAILABLE IN THE MAIN SELECT CASE - CHECK THE Input FILE" ;
          Write(*,*) ;
          Write(UnInf,*) ;
          Write(*,*)' Program TERMINATED DUE TO SOME TECHNICAL PROBLEM - CHECK THE .INF FILE FOR FURHTER INFORMATION' ;
          !#Call BEEP_FAIL ;
          Write(*, Fmt_End) ; Read(*,*) ;  STOP ;
          !Read(*," ('PRESS ENTER TO End ...') " ) ;
          !STOP

      End SELECT ;

    DeAllocate ( Param%IntL ,Param%RealL ) ;   !?? modify the allocation numbers

    ! =========================== RUNNING TIME OF THE CODE ==============================================================================================
    Call CPU_TIME ( TimeE ) ;
    Call Info ( NEl, TimeE, TimeS, TimeInputE, TimeInputS, TimeIndexE, TimeIndexS, TimeAssemE, TimeAssemS, TimeEStiffE, TimeEStiffS, TimeSolveE, TimeSolveS ) ;

    ! =========================== End Simulation  =======================================================================================================
    !#Call BEEP_SUCC
    Write (*,*)"Simulation done successfully for:" ;
    Write (*,"(' Model Name: ',A30,'Analysis Name: ', A30)")ModelName, AnaName ;
    Write (*,*) ;

    ! =========================== Close FILES ===========================================================================================================
    ! - Close Data FILE ---------------------------------------------------------------------------------------------------------------------------------
    ! Data file
    UnFile= UnInptAna ;
    Close ( Unit=UnFile, STATUS='KEEP', ERR=1002, IOSTAT=IO_File ) ;

    ! - Close Data check file ---------------------------------------------------------------------------------------------------------------------------
    ! check file
    UnFile= Un_CHK ;
    Close ( Unit=UnFile, STATUS='KEEP', ERR=1002, IOSTAT=IO_File ) ;

  End Do Analysis_Loop ;

! - Close ADDRESS FILE ------------------------------------------------------------------------------------------------------------------------------
UnFile= UN_ADR ;
Close ( Unit=UnFile, STATUS='KEEP', ERR=1002, IOSTAT=IO_File ) ;

! - Close IFORMATION FILE ---------------------------------------------------------------------------------------------------------------------------
UnFile= UnInf ;
Close ( Unit=UnFile, STATUS='KEEP', ERR=1002, IOSTAT=IO_File ) ;

! - SHUT DOWN PETSC ---------------------------------------------------------------------------------------------------------------------------------
Write (*, "('Simulation is done!')") ;
Write (*, "('Shut down PETSc ...')") ;

  SELECT CASE ( NAT ) ;

! ---------------------------------------------------------------------------------------------------------------------------------------------------
! - Analytical solution of wave propagation in half-space
! ---------------------------------------------------------------------------------------------------------------------------------------------------
    CASE ( ACN_Analytical_DRM ) ;    ! # 73

! ---------------------------------------------------------------------------------------------------------------------------------------------------
! -  Standard FEM  Implicit 2D
! ---------------------------------------------------------------------------------------------------------------------------------------------------
    CASE ( ACN_FEM_Implicit_2D ) ;    ! # 61

      ! - SHUT DOWN PETSc
      Call MatDestroy   (   K_PTC, ErrPTC ) ;
      Call MatDestroy   (   M_PTC, ErrPTC ) ;
      Call MatDestroy   (   C_PTC, ErrPTC ) ;

        If ( Param%IntL( 6, 5) == 0_Tiny ) Then ;  ! NEnergy
          Call MatDestroy   (   K_En_PTC, ErrPTC ) ;
          Call MatDestroy   (   M_En_PTC, ErrPTC ) ;
        End If ;

      If ( LoadC (1) /= 0_Smll )  Call VecDestroy ( PBF_PTC, ErrPTC ) ;
      If ( LoadC (2) /= 0_Smll )  Call VecDestroy ( PPR_PTC, ErrPTC ) ;
      If ( LoadC (3) /= 0_Smll )  Call VecDestroy ( PCF_PTC, ErrPTC ) ;
      If ( LoadC (4) /= 0_Smll )  Call VecDestroy ( PD_PTC,  ErrPTC ) ;
      If ( LoadC (5) /= 0_Smll )  Call VecDestroy ( PDY_PTC, ErrPTC ) ;

      ! DeAllocating the arrays
      DEAllocate( NDAN, NVAN, NAAN, ELGR, STEP,     ID_Para, idx_from,      NEG, INOD, ID,       XYZ, PMat, BACL,     STAT=ERR_DeAlloc ) ;
        IF ( ERR_DeAlloc /= 0 ) Then ;
          Write (*, Fmt_DEALLCT) ERR_DeAlloc ;  Write (UnInf, Fmt_DEALLCT) ERR_DeAlloc ;
          Write(*, Fmt_FL) ;  Write(UnInf, Fmt_FL) ; Write(*, Fmt_End) ; Read(*,*) ;  STOP ;
        End If ;

! ---------------------------------------------------------------------------------------------------------------------------------------------------
! - Spectral Implicit 2D DRM
! ---------------------------------------------------------------------------------------------------------------------------------------------------
    CASE ( ACN_FEM_Implicit_2D_DRM ) ;    ! # 64

      ! - SHUT DOWN PETSc ---------------------------------------------------------------------------------------------------------------------------
      Call MatDestroy   (   K_PTC, ErrPTC ) ;
      Call MatDestroy   (   M_PTC, ErrPTC ) ;
      Call MatDestroy   (   C_PTC, ErrPTC ) ;

      Call MatDestroy   (   K_eb_PTC, ErrPTC ) ;
      Call MatDestroy   (   M_eb_PTC, ErrPTC ) ;
      Call MatDestroy   (   C_eb_PTC, ErrPTC ) ;

        If ( Param%IntM( 6, 5) == 0_Tiny ) Then ;  ! NEnergy
          Call MatDestroy   (   K_En_PTC, ErrPTC ) ;
          Call MatDestroy   (   M_En_PTC, ErrPTC ) ;
        End If ;

      If ( LoadC (1) /= 0_Smll )  Call VecDestroy ( PBF_PTC, ErrPTC ) ;
      If ( LoadC (2) /= 0_Smll )  Call VecDestroy ( PPR_PTC, ErrPTC ) ;
      If ( LoadC (3) /= 0_Smll )  Call VecDestroy ( PCF_PTC, ErrPTC ) ;
      If ( LoadC (4) /= 0_Smll )  Call VecDestroy ( PD_PTC,  ErrPTC ) ;
      If ( LoadC (5) /= 0_Smll )  Call VecDestroy ( PDY_PTC, ErrPTC ) ;

      ! DeAllocating the arrays
      DEAllocate( NDAN, NVAN, NAAN, ELGR, STEP,     ID_Para, idx_from,      NEG, INOD, ID,       XYZ, PMat, BACL, ND_b, ND_e,     STAT=ERR_DeAlloc ) ;
        IF ( ERR_DeAlloc /= 0 ) Then ;
          Write (*, Fmt_DEALLCT) ERR_DeAlloc ;  Write (UnInf, Fmt_DEALLCT) ERR_DeAlloc ;
          Write(*, Fmt_FL) ;  Write(UnInf, Fmt_FL) ; Write(*, Fmt_End) ; Read(*,*) ;  STOP ;
        End If ;

! ---------------------------------------------------------------------------------------------------------------------------------------------------
! - Spectral Explicit 2D - No DRM
! ---------------------------------------------------------------------------------------------------------------------------------------------------
    CASE ( ACN_Spec_Explicit_2D ) ;    ! # 63

      ! - SHUT DOWN PETSc
      Call MatDestroy   (   K_PTC, ErrPTC ) ;
      Call MatDestroy   (   C_PTC, ErrPTC ) ;

      Call VecDestroy ( VM_PTC, ErrPTC ) ;

        If ( Param%IntL( 6, 5) == 0_Tiny ) Then ;  ! NEnergy
          Call MatDestroy   (   K_En_PTC, ErrPTC ) ;
          Call MatDestroy   (   M_En_PTC, ErrPTC ) ;
        End If ;

      If ( LoadC (1) /= 0_Smll )  Call VecDestroy ( PBF_PTC, ErrPTC ) ;
      If ( LoadC (2) /= 0_Smll )  Call VecDestroy ( PPR_PTC, ErrPTC ) ;
      If ( LoadC (3) /= 0_Smll )  Call VecDestroy ( PCF_PTC, ErrPTC ) ;
      If ( LoadC (4) /= 0_Smll )  Call VecDestroy ( PD_PTC,  ErrPTC ) ;
      If ( LoadC (5) /= 0_Smll )  Call VecDestroy ( PDY_PTC, ErrPTC ) ;

      ! DeAllocating the arrays
      DEAllocate( NDAN, NVAN, NAAN, ELGR, STEP,     ID_Para, idx_from,      NEG, INOD, ID,       XYZ, PMat, BACL,     STAT=ERR_DeAlloc ) ;
        IF ( ERR_DeAlloc /= 0 ) Then ;
          Write (*, Fmt_DEALLCT) ERR_DeAlloc ;  Write (UnInf, Fmt_DEALLCT) ERR_DeAlloc ;
          Write(*, Fmt_FL) ;  Write(UnInf, Fmt_FL) ; Write(*, Fmt_End) ; Read(*,*) ;  STOP ;
        End If ;

! ---------------------------------------------------------------------------------------------------------------------------------------------------
! - Spectral Explicit 3D
! ---------------------------------------------------------------------------------------------------------------------------------------------------
    CASE ( ACN_Spec_Explicit_3D ) ;    ! # 69

      ! - SHUT DOWN PETSc
      Call MatDestroy   (   K_PTC, ErrPTC ) ;
      Call MatDestroy   (   C_PTC, ErrPTC ) ;
      Call MatDestroy   (   G_PTC, ErrPTC ) ;

      Call VecDestroy ( VM_PTC, ErrPTC ) ;

        If ( Param%IntL( 6, 5) == 0_Tiny ) Then ;  ! NEnergy
          Call MatDestroy   (   K_En_PTC, ErrPTC ) ;
          Call MatDestroy   (   M_En_PTC, ErrPTC ) ;
        End If ;

      If ( LoadC (1) /= 0_Smll )  Call VecDestroy ( PBF_PTC, ErrPTC ) ;
      If ( LoadC (2) /= 0_Smll )  Call VecDestroy ( PPR_PTC, ErrPTC ) ;
      If ( LoadC (3) /= 0_Smll )  Call VecDestroy ( PCF_PTC, ErrPTC ) ;
      If ( LoadC (4) /= 0_Smll )  Call VecDestroy ( PD_PTC,  ErrPTC ) ;
      If ( LoadC (5) /= 0_Smll )  Call VecDestroy ( PDY_PTC, ErrPTC ) ;

      ! DeAllocating the arrays
      DEAllocate( NDAN, NVAN, NAAN, ELGR, STEP, JLoad, PLoad,     ID_Para, idx_from,      NEG, INOD, ID,       XYZ, PMat, BACL,     STAT=ERR_DeAlloc ) ;
        IF ( ERR_DeAlloc /= 0 ) Then ;
          Write (*, Fmt_DEALLCT) ERR_DeAlloc ;  Write (UnInf, Fmt_DEALLCT) ERR_DeAlloc ;
          Write(*, Fmt_FL) ;  Write(UnInf, Fmt_FL) ; Write(*, Fmt_End) ; Read(*,*) ;  STOP ;
        End If ;

! ---------------------------------------------------------------------------------------------------------------------------------------------------
! - Spectral Explicit 2D DRM
! ---------------------------------------------------------------------------------------------------------------------------------------------------
    CASE ( ACN_Spec_Explicit_2D_DRM ) ;    ! # 66

      ! - SHUT DOWN PETSc ---------------------------------------------------------------------------------------------------------------------------
      Write(*,*)"Dealocate PETSc Matrices ..." ;

      Call MatDestroy   (   K_PTC, ErrPTC ) ;
     !Call MatDestroy   (   M_PTC, ErrPTC ) ;
      Call MatDestroy   (   C_PTC, ErrPTC ) ;

      !Call MatDestroy   (   K_eb_PTC, ErrPTC ) ;
      !Call MatDestroy   (   M_eb_PTC, ErrPTC ) ;
      !Call MatDestroy   (   C_eb_PTC, ErrPTC ) ;

!        If ( Param%IntL( 6, 5) == 0_Tiny ) Then ;  ! NEnergy
!          Call MatDestroy   (   K_En_PTC, ErrPTC ) ;
!          Call MatDestroy   (   M_En_PTC, ErrPTC ) ;
!        End If ;

      Write(*,*)"Dealocate PETSc Vectors ..." ;
      Call VecDestroy ( VM_PTC, ErrPTC ) ;
      If ( LoadC (1) /= 0_Smll )  Call VecDestroy ( PBF_PTC, ErrPTC ) ;
      If ( LoadC (2) /= 0_Smll )  Call VecDestroy ( PPR_PTC, ErrPTC ) ;
      If ( LoadC (3) /= 0_Smll )  Call VecDestroy ( PCF_PTC, ErrPTC ) ;
      If ( LoadC (4) /= 0_Smll )  Call VecDestroy ( PD_PTC,  ErrPTC ) ;
      If ( LoadC (6) /= 0_Smll )  Call VecDestroy ( PDY_PTC, ErrPTC ) ;

      ! DeAllocating the arrays
      Write(*,*)"Dealocate Matrices ..." ;
      DEAllocate( NDAN, NVAN, NAAN, ELGR, STEP,     ID_Para, idx_from,      INOD, ID,       XYZ, PMat, InciWave, ND_b, ND_e,     STAT=ERR_DeAlloc ) ;  ! NEG, BACL, 
        IF ( ERR_DeAlloc /= 0 ) Then ;
          Write (*, Fmt_DEALLCT) ERR_DeAlloc ;  Write (UnInf, Fmt_DEALLCT) ERR_DeAlloc ;
          Write(*, Fmt_FL) ;  Write(UnInf, Fmt_FL) ; Write(*, Fmt_End) ; Read(*,*) ;  STOP ;
        End If ;

    If ( Param%IntM( 5, 3 ) == 3_Tiny ) Then ; 
      DEAllocate( NoBndry_DRM, NoLayer_DRM,      STAT=ERR_DeAlloc ) ;
        IF ( ERR_DeAlloc /= 0 ) Then ;
          Write (*, Fmt_DEALLCT) ERR_DeAlloc ;  Write (UnInf, Fmt_DEALLCT) ERR_DeAlloc ;
          Write(*, Fmt_FL) ;  Write(UnInf, Fmt_FL) ; Write(*, Fmt_End) ; Read(*,*) ;  STOP ;
        End If ;
    End If ;

! ---------------------------------------------------------------------------------------------------------------------------------------------------
! - Spectral method Explicit 3D DRM
! ---------------------------------------------------------------------------------------------------------------------------------------------------
    CASE ( ACN_Spec_Explicit_3D_DRM ) ;    ! # 72

      ! - SHUT DOWN PETSc
      Call MatDestroy   (   K_PTC, ErrPTC ) ;
      Call MatDestroy   (   C_PTC, ErrPTC ) ;
      Call MatDestroy   (   G_PTC, ErrPTC ) ;

      Call MatDestroy   (   K_eb_PTC, ErrPTC ) ;
      !Call MatDestroy   (   M_eb_PTC, ErrPTC ) ;
      Call MatDestroy   (   C_eb_PTC, ErrPTC ) ;

      Call VecDestroy ( VM_PTC, ErrPTC ) ;
      If ( LoadC (1) /= 0_Smll )  Call VecDestroy ( PBF_PTC, ErrPTC ) ;
      If ( LoadC (2) /= 0_Smll )  Call VecDestroy ( PPR_PTC, ErrPTC ) ;
      If ( LoadC (3) /= 0_Smll )  Call VecDestroy ( PCF_PTC, ErrPTC ) ;
      If ( LoadC (4) /= 0_Smll )  Call VecDestroy ( PD_PTC,  ErrPTC ) ;
      If ( LoadC (5) /= 0_Smll )  Call VecDestroy ( PDY_PTC, ErrPTC ) ;

!        If ( Param%IntL( 6, 5) == 0_Tiny ) Then ;  ! NEnergy
!          Call MatDestroy   (   K_En_PTC, ErrPTC ) ;
!          Call MatDestroy   (   M_En_PTC, ErrPTC ) ;
!        End If ;

      ! DeAllocating the arrays
      DEAllocate( NDAN, NVAN, NAAN, ELGR, STEP,     ID_Para, idx_from,      INOD, ID,       XYZ, PMat, InciWave, ND_e, ND_b,     STAT=ERR_DeAlloc ) ;
        IF ( ERR_DeAlloc /= 0 ) Then ;
          Write (*, Fmt_DEALLCT) ERR_DeAlloc ;  Write (UnInf, Fmt_DEALLCT) ERR_DeAlloc ;
          Write(*, Fmt_FL) ;  Write(UnInf, Fmt_FL) ; Write(*, Fmt_End) ; Read(*,*) ;  STOP ;
        End If ;

    If ( Param%IntM( 5, 3 ) == 3_Tiny ) Then ; 
      DEAllocate( NoBndry_DRM, NoLayer_DRM,      STAT=ERR_DeAlloc ) ;
        IF ( ERR_DeAlloc /= 0 ) Then ;
          Write (*, Fmt_DEALLCT) ERR_DeAlloc ;  Write (UnInf, Fmt_DEALLCT) ERR_DeAlloc ;
          Write(*, Fmt_FL) ;  Write(UnInf, Fmt_FL) ; Write(*, Fmt_End) ; Read(*,*) ;  STOP ;
        End If ;
    End If ;


! ---------------------------------------------------------------------------------------------------------------------------------------------------
! - Spectral method Explicit 3D DRM
! ---------------------------------------------------------------------------------------------------------------------------------------------------
    CASE ( ACN_Spec_Explicit_3D_DC ) ;    ! # 75

      ! - SHUT DOWN PETSc
      Call MatDestroy   (   K_PTC, ErrPTC ) ;
      Call MatDestroy   (   C_PTC, ErrPTC ) ;
      Call MatDestroy   (   G_PTC, ErrPTC ) ;

      Call MatDestroy   (   K_eb_PTC, ErrPTC ) ;
      !Call MatDestroy   (   M_eb_PTC, ErrPTC ) ;
      Call MatDestroy   (   C_eb_PTC, ErrPTC ) ;

      Call VecDestroy ( VM_PTC, ErrPTC ) ;
      If ( LoadC (1) /= 0_Smll )  Call VecDestroy ( PBF_PTC, ErrPTC ) ;
      If ( LoadC (2) /= 0_Smll )  Call VecDestroy ( PPR_PTC, ErrPTC ) ;
      If ( LoadC (3) /= 0_Smll )  Call VecDestroy ( PCF_PTC, ErrPTC ) ;
      If ( LoadC (4) /= 0_Smll )  Call VecDestroy ( PD_PTC,  ErrPTC ) ;
      If ( LoadC (5) /= 0_Smll )  Call VecDestroy ( PDY_PTC, ErrPTC ) ;

      ! DeAllocating the arrays
      DEAllocate( NDAN, NVAN, NAAN, ELGR, STEP,     ID_Para, idx_from,      INOD, ID,       XYZ, PMat, DoubleCouple_El, ND_DC,     STAT=ERR_DeAlloc ) ;
        IF ( ERR_DeAlloc /= 0 ) Then ;
          Write (*, Fmt_DEALLCT) ERR_DeAlloc ;  Write (UnInf, Fmt_DEALLCT) ERR_DeAlloc ;
          Write(*, Fmt_FL) ;  Write(UnInf, Fmt_FL) ; Write(*, Fmt_End) ; Read(*,*) ;  STOP ;
        End If ;

! ---------------------------------------------------------------------------------------------------------------------------------------------------
! - Spectral method Implicit 2D
! ---------------------------------------------------------------------------------------------------------------------------------------------------
    CASE ( ACN_Spec_Implicit_2D ) ;    ! # 62

      ! - SHUT DOWN PETSc
      Call MatDestroy   (   K_PTC, ErrPTC ) ;
      Call MatDestroy   (   M_PTC, ErrPTC ) ;
      Call MatDestroy   (   C_PTC, ErrPTC ) ;

      Call VecDestroy ( VM_PTC, ErrPTC ) ;

        If ( Param%IntL( 6, 5) == 0_Tiny ) Then ;  ! NEnergy
          Call MatDestroy   (   K_En_PTC, ErrPTC ) ;
          Call MatDestroy   (   M_En_PTC, ErrPTC ) ;
        End If ;

      If ( LoadC (1) /= 0_Smll )  Call VecDestroy ( PBF_PTC, ErrPTC ) ;
      If ( LoadC (2) /= 0_Smll )  Call VecDestroy ( PPR_PTC, ErrPTC ) ;
      If ( LoadC (3) /= 0_Smll )  Call VecDestroy ( PCF_PTC, ErrPTC ) ;
      If ( LoadC (4) /= 0_Smll )  Call VecDestroy ( PD_PTC,  ErrPTC ) ;
      If ( LoadC (5) /= 0_Smll )  Call VecDestroy ( PDY_PTC, ErrPTC ) ;

      ! DeAllocating the arrays
      DEAllocate( NDAN, NVAN, NAAN, ELGR, STEP,     ID_Para, idx_from,      NEG, INOD, ID,       XYZ, PMat, BACL,     STAT=ERR_DeAlloc ) ;
        IF ( ERR_DeAlloc /= 0 ) Then ;
          Write (*, Fmt_DEALLCT) ERR_DeAlloc ;  Write (UnInf, Fmt_DEALLCT) ERR_DeAlloc ;
          Write(*, Fmt_FL) ;  Write(UnInf, Fmt_FL) ; Write(*, Fmt_End) ; Read(*,*) ;  STOP ;
        End If ;

    ! - ERROR IN ANALYSIS NUMBERING ---------------------------------------------------------------------------------------------------------------------
    CASE DEFAULT ;
      Write(*,*)" Type OF ANALYSIS IS NOT AVAILABLE IN THE MAIN SELECT CASE - CHECK THE Input FILE" ;
      Write(UnInf,*)" Type OF ANALYSIS IS NOT AVAILABLE IN THE MAIN SELECT CASE - CHECK THE Input FILE" ;
      Write(*,*) ;
      Write(UnInf,*) ;
      Write(*,*)' Program TERMINATED DUE TO SOME TECHNICAL PROBLEMs - CHECK THE .INF FILE FOR FURHTER INFORMATION' ;
      !#Call BEEP_FAIL ;
      Write(*, Fmt_End) ; Read(*,*) ;  STOP ;
      !Read(*," ('PRESS ENTER TO End ...') " ) ;
      !STOP

  End SELECT ;

Call AODestroy ( AOOut, ErrPTC ) ;
!Call ISDestroy ( ISDis, ErrPTC ) ;
!Call ISDestroy ( ISStr, ErrPTC ) ;
Call ISLocalToGlobalMappingDestroy ( GMapping, ErrPTC ) ;
Call PetscFinalize ( ErrPTC ) ;

! =========================== FINISH THE CODE =======================================================================================================
!#Call BEEP_SUCC
Write (*,"('Model Name: ',A30,'Analysis Name: ', A30)")ModelName, AnaName ;
Write (*, Fmt_SUC) ;  Write(UnInf, Fmt_SUC) ;
Write (*, Fmt_End) ;

!#Read(*,*);
STOP ;

! =============================================== OPEN ERRORS =======================================================================================
1001  IF ( IO_File > 0 ) Then ;
        Write(*, Fmt_ERR1_OPEN) UnFile, IO_File  ;  Write(UnInf, Fmt_ERR1_OPEN) UnFile, IO_File  ; 
        Write(*, Fmt_FL) ; Write(UnInf, Fmt_FL) ;
        !#Call BEEP_FAIL ;
        Write(*, Fmt_End) ; Read(*,*) ;  STOP ;
      Else If ( IO_File < 0 ) Then ;
        Write(*, Fmt_ERR1_OPEN) UnFile, IO_File  ; 
        Write(UnInf, Fmt_ERR1_OPEN) UnFile, IO_File  ;  Write(*, Fmt_FL) ; Write(UnInf, Fmt_FL) ;
        !#Call BEEP_FAIL ;
        Write(*, Fmt_End) ; Read(*,*) ;  STOP ;
      End If ;


! =============================================== Close ERRORS ======================================================================================
1002  IF ( IO_File > 0 ) Then ;
        Write(*, Fmt_ERR1_Close) UnFile, IO_File  ;  Write(UnInf, Fmt_ERR1_Close) UnFile, IO_File  ; 
        Write(*, Fmt_FL) ; Write(UnInf, Fmt_FL) ;
        !Call BEEP_FAIL ;
        Write(*, Fmt_End) ; Read(*,*) ; ; STOP ;
      End If ;

End Program FINITE_ELEMENTS_PTS ;


end program SVE;