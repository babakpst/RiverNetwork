  ¤#  G   k820309              18.0        Z                                                                                                          
       Parameters_mod.f90 PARAMETERS                                                                                                                                                       SELECTED_REAL_KIND                                                                                                                                                                                                                                                            SELECTED_INT_KIND                                                                                                                                                                                                                                                                                                                                                            	     
                
                 -DTû!	@        3.141592653589793238                                            
     W                            X                       C(' DATE :  ',I2.2,' - ',I2.2,' - ',I4,/,' TIME : ',I2.2,':',I2.2,':',I2.2,':',I2.2,/ )                                                                                                                     C('PRESS ENTER TO End ...')                                                                  P                            Q                       C( 'ERROR IN OPEN STATEMENT. Unit NUMBER=', I3, '   ERROR NUMBER IS=', I4  )                                                                      [                            \                       C('End-OF-FILE ERROR IN OPEN STATEMENT. Unit NUMBER=', I3, '   ERROR NUMBER IS=', I4  )                                                                      Q                            R                       C( 'ERROR IN Close STATEMENT. Unit NUMBER=', I3, '   ERROR NUMBER IS=', I4  )                                                                      £                            ¤                       C(' FILE Name : ', A20,//,' Directories :',/, 'INPUT FILE DIRECTORY     : ', A100,/, 'OUTPUT FILES DIRECTORY   : ', A100 )                                                                                                           )                            *                       C('CONGRATULATIONS! DONE SUCCESSFULLY. ')                                                                  '                            (                       C('OOPS!!!  FAIL TO OPERATE PROPERLY.')                                                                  N                            O                       C('ERROR IN ALLOCATING Arrays. ERROR NUMBER IS :', I4, '   LOCATION: ??????.')                                                                  P                            Q                       C('ERROR IN DEALLOCATING Arrays. ERROR NUMBER IS :', I4, '   LOCATION: ??????.')                                                                                                                     C(A,F50.2,'   SECONDS')                                                                  F                            G                       C('ERROR IN READ STATEMENT. Unit IS : ',I5,' ERROR NUMBER IS : ', I5 )                                                                  L                            M                       C('End OF FILE IN READ STATEMENT. Unit IS : ',I5,' ERROR NUMBER IS : ', I5 )                                                                  N                            O                       C('End OF RECORD IN READ STATEMENT. Unit IS : ',I5,' ERROR NUMBER IS : ', I5 )                                                                  G                            H                       C('ERROR IN write STATEMENT. Unit IS : ',I5,' ERROR NUMBER IS : ', I5 )                                                                                                                     C('Error in the element type. Either there is a mistake in the input file for element type or element type in not available in the code yet.')                                                                                                                      C('Error in the element type. This element number',I3,'is not available in the list of this code. Check the input file for element number',I19)                                                                                                     ô              500                                                                                   õ              501                                                                                   ö              502                                                                                   ÷              503                                                                                   ø              504                                                                                    þ              510                                            !                                       ÿ              511                                            "                                       W              599                                            #                                       X              600                                            $                                       d               100                  @                           %     '                    #YEAR &   #MONTH '   #DAY (   #HOUR )   #MINUTE *   #SECONDS +   #S100TH ,                                              &                                                              '                                                             (                                                             )                                                             *                                                             +     
                                                        ,                                    @               @           -     'È                   #MODELNAME .   #INPUTDIR /   #ANALYSISDIR 0   #OUTPUTDIR 1   #ANALYSISOUTPUTDIR 2   #ANALYSESNAMES 3   #ANALYSISTYPE 4   #NUMBEROFANALYSES 5                                              .                                                                      /                                                                     0            ´                                                         1            J                                                        2            à             .                                         3            x                            &                                                                                              4     À                                                       5     Â                              @               @           6     'à                    #ARGCOUNT 7   #ARGSTATUS 8   #LENGTH 9   #ARG :                                              7                                                            8                                         &                                                                                    9            P                             &                                           .                                         :                   2                      &                                                                     @                           ;     '                     #TIME_START <   #TIME_END =   #INPUT_STARTS >   #INPUT_ENDS ?                                              <                
                                              =               
                                              >               
                                              ?               
                     @                           @     '0                    #TOTALTIME A   #TIMESTEP B   #Q_UP C   #H_DW D   #CNTRLV E   #NOREACHES F                                              A                
                                              B               
                                              C               
                                              D               
                                              E                
                                              F     (          
          &      fn#fn    Æ   p       SGL #   6  K       SELECTED_REAL_KIND      p       DBL    ñ  p       TINY "   a  J       SELECTED_INT_KIND    «  p       SMLL      p       SHRT      p       LNG    û         PI      Ø       FMT_DATE    W         FMT_END    ó  Ñ       FMT_ERR1_OPEN    Ä  Ü       FMT_ERR2_OPEN       Ò       FMT_ERR1_CLOSE    r  $      FMT_NM    	  ª       FMT_SUC    @
  ¨       FMT_FL    è
  Ï       FMT_ALLCT    ·  Ñ       FMT_DEALLCT             FMT_RUNTIME       Ç       FMT_READ1    ç  Í       FMT_READ2    ´  Ï       FMT_READ3      È       FMT_WRITE1    K        FMT_ELEMENT1    [        FMT_ELEMENT2    l  s       FILEADR    ß  s       FILEDATAMODEL    R  s       UNINPTXYZ    Å  s       UNINPTCNN    8  s       UNINPTCNT    «  s       UNINPTANA      s       UNINPTMAT      s       UN_CHK      s       FILEINFO     w  s       ANALYSISTYPE_1D    ê         TIMEDATE_TP !     H   a   TIMEDATE_TP%YEAR "   Ï  H   a   TIMEDATE_TP%MONTH       H   a   TIMEDATE_TP%DAY !   _  H   a   TIMEDATE_TP%HOUR #   §  H   a   TIMEDATE_TP%MINUTE $   ï  H   a   TIMEDATE_TP%SECONDS #   7  H   a   TIMEDATE_TP%S100TH      ß       INPUT_DATA_TP (   ^  P   a   INPUT_DATA_TP%MODELNAME '   ®  P   a   INPUT_DATA_TP%INPUTDIR *   þ  P   a   INPUT_DATA_TP%ANALYSISDIR (   N  P   a   INPUT_DATA_TP%OUTPUTDIR 0     P   a   INPUT_DATA_TP%ANALYSISOUTPUTDIR ,   î     a   INPUT_DATA_TP%ANALYSESNAMES +     H   a   INPUT_DATA_TP%ANALYSISTYPE /   Ò  H   a   INPUT_DATA_TP%NUMBEROFANALYSES             ARGCOMMANDS %     H   a   ARGCOMMANDS%ARGCOUNT &   ä     a   ARGCOMMANDS%ARGSTATUS #   x     a   ARGCOMMANDS%LENGTH          a   ARGCOMMANDS%ARG    ¨         TIMING "   8   H   a   TIMING%TIME_START        H   a   TIMING%TIME_END $   È   H   a   TIMING%INPUT_STARTS "   !  H   a   TIMING%INPUT_ENDS    X!         INITIALDATA_TP )   ô!  H   a   INITIALDATA_TP%TOTALTIME (   <"  H   a   INITIALDATA_TP%TIMESTEP $   "  H   a   INITIALDATA_TP%Q_UP $   Ì"  H   a   INITIALDATA_TP%H_DW &   #  H   a   INITIALDATA_TP%CNTRLV )   \#  H   a   INITIALDATA_TP%NOREACHES 