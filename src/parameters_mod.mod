  '  N   k820309              18.0        È/Z                                                                                                          
       Parameters_mod.f90 PARAMETERS_MOD                                                                                                                                                       SELECTED_REAL_KIND                                                                                                                                                                                                                                                            SELECTED_INT_KIND                                                                                                                                                                                                                                                                                                                                                            	     
                
                 -DTû!	@        3.141592653589793238                                            
     W                            X                       C(' DATE :  ',I2.2,' - ',I2.2,' - ',I4,/,' TIME : ',I2.2,':',I2.2,':',I2.2,':',I2.2,/ )                                                                                                                     C('PRESS ENTER TO End ...')                                                                  P                            Q                       C( 'ERROR IN OPEN STATEMENT. Unit NUMBER=', I3, '   ERROR NUMBER IS=', I4  )                                                                      [                            \                       C('End-OF-FILE ERROR IN OPEN STATEMENT. Unit NUMBER=', I3, '   ERROR NUMBER IS=', I4  )                                                                      Q                            R                       C( 'ERROR IN Close STATEMENT. Unit NUMBER=', I3, '   ERROR NUMBER IS=', I4  )                                                                      £                            ¤                       C(' FILE Name : ', A20,//,' Directories :',/, 'INPUT FILE DIRECTORY     : ', A100,/, 'OUTPUT FILES DIRECTORY   : ', A100 )                                                                                                           )                            *                       C('CONGRATULATIONS! DONE SUCCESSFULLY. ')                                                                  '                            (                       C('OOPS!!!  FAIL TO OPERATE PROPERLY.')                                                                  N                            O                       C('ERROR IN ALLOCATING Arrays. ERROR NUMBER IS :', I4, '   LOCATION: ??????.')                                                                  P                            Q                       C('ERROR IN DEALLOCATING Arrays. ERROR NUMBER IS :', I4, '   LOCATION: ??????.')                                                                                                                     C(A,F50.2,'   SECONDS')                                                                  F                            G                       C('ERROR IN READ STATEMENT. Unit IS : ',I5,' ERROR NUMBER IS : ', I5 )                                                                  L                            M                       C('End OF FILE IN READ STATEMENT. Unit IS : ',I5,' ERROR NUMBER IS : ', I5 )                                                                  N                            O                       C('End OF RECORD IN READ STATEMENT. Unit IS : ',I5,' ERROR NUMBER IS : ', I5 )                                                                  G                            H                       C('ERROR IN write STATEMENT. Unit IS : ',I5,' ERROR NUMBER IS : ', I5 )                                                                                                                     C('Error in the element type. Either there is a mistake in the input file for element type or element type in not available in the code yet.')                                                                                                                      C('Error in the element type. This element number',I3,'is not available in the list of this code. Check the input file for element number',I19)                                                                                                     ô              500                                                                                   õ              501                                                                                   ö              502                                                                                   þ              510                                                                                   ÿ              511                                                                                    W              599                                            !                                       X              600                                            "                                       d               100                  @                           #     '                    #YEAR $   #MONTH %   #DAY &   #HOUR '   #MINUTE (   #SECONDS )   #S100TH *                                              $                                                              %                                                             &                                                             '                                                             (                                                             )     
                                                        *                                    @               @           +     'È             	      #MODELNAME ,   #INPUTDIR -   #ANALYSISDIR .   #OUTPUTDIR /   #ANALYSISOUTPUTDIR 0   #ANALYSESNAMES 1   #ANALYSISTYPE 2   #NUMBEROFANALYSES 3   #VERSION 4                                              ,                                                                      -                                                                     .            ´                                                         /            J                                                        0            à             .                                         1            x                            &                                                                                              2     À                                                       3     Â                                                       4     Ä      	   	                     @               @           5     'à                    #ARGCOUNT 6   #ARGSTATUS 7   #LENGTH 8   #ARG 9                                              6                                                            7                                         &                                                                                    8            P                             &                                           .                                         9                   2                      &                                                                     @                           :     '                     #TIME_START ;   #TIME_END <   #INPUT_STARTS =   #INPUT_ENDS >                                              ;                
                                              <               
                                              =               
                                              >               
                     @                           ?     '8                    #NOREACHES @   #TOTALTIME A   #TIMESTEP B   #Q_UP C   #H_DW D   #CNTRLV E   #CNTRLV_RATIO F                                              @                                                              A               
                                              B               
                                              C               
                                              D                
                                              E     (          
                                              F     0          
                     @               @           G     '°                   #REACHDISC H   #REACHLENGTH I   #REACHTYPE J   #REACHSLOPE K   #REACHMANNING L   #REACHWIDTH M                                            H                                          &                                                                                    I            H                 
            &                                                                                    J                             
            &                                                                                    K            Ø                 
            &                                                                                    L                             
            &                                                                                    M            h                
            &                                                  *      fn#fn    Ê   p       SGL #   :  K       SELECTED_REAL_KIND      p       DBL    õ  p       TINY "   e  J       SELECTED_INT_KIND    ¯  p       SMLL      p       SHRT      p       LNG    ÿ         PI      Ø       FMT_DATE    [         FMT_END    ÷  Ñ       FMT_ERR1_OPEN    È  Ü       FMT_ERR2_OPEN    ¤  Ò       FMT_ERR1_CLOSE    v  $      FMT_NM    	  ª       FMT_SUC    D
  ¨       FMT_FL    ì
  Ï       FMT_ALLCT    »  Ñ       FMT_DEALLCT             FMT_RUNTIME    $  Ç       FMT_READ1    ë  Í       FMT_READ2    ¸  Ï       FMT_READ3      È       FMT_WRITE1    O        FMT_ELEMENT1    _        FMT_ELEMENT2    p  s       FILEADR    ã  s       FILEDATAMODEL    V  s       FILEDATAGEO    É  s       UNINPTANA    <  s       UNINPTMAT    ¯  s       UN_CHK    "  s       FILEINFO       s       ANALYSISTYPE_1D             TIMEDATE_TP !   ¥  H   a   TIMEDATE_TP%YEAR "   í  H   a   TIMEDATE_TP%MONTH     5  H   a   TIMEDATE_TP%DAY !   }  H   a   TIMEDATE_TP%HOUR #   Å  H   a   TIMEDATE_TP%MINUTE $     H   a   TIMEDATE_TP%SECONDS #   U  H   a   TIMEDATE_TP%S100TH      ì       INPUT_DATA_TP (     P   a   INPUT_DATA_TP%MODELNAME '   Ù  P   a   INPUT_DATA_TP%INPUTDIR *   )  P   a   INPUT_DATA_TP%ANALYSISDIR (   y  P   a   INPUT_DATA_TP%OUTPUTDIR 0   É  P   a   INPUT_DATA_TP%ANALYSISOUTPUTDIR ,        a   INPUT_DATA_TP%ANALYSESNAMES +   µ  H   a   INPUT_DATA_TP%ANALYSISTYPE /   ý  H   a   INPUT_DATA_TP%NUMBEROFANALYSES &   E  H   a   INPUT_DATA_TP%VERSION             ARGCOMMANDS %     H   a   ARGCOMMANDS%ARGCOUNT &   W     a   ARGCOMMANDS%ARGSTATUS #   ë     a   ARGCOMMANDS%LENGTH          a   ARGCOMMANDS%ARG             TIMING "   «  H   a   TIMING%TIME_START     ó  H   a   TIMING%TIME_END $   ;   H   a   TIMING%INPUT_STARTS "      H   a   TIMING%INPUT_ENDS    Ë   ®       INITIALDATA_TP )   y!  H   a   INITIALDATA_TP%NOREACHES )   Á!  H   a   INITIALDATA_TP%TOTALTIME (   	"  H   a   INITIALDATA_TP%TIMESTEP $   Q"  H   a   INITIALDATA_TP%Q_UP $   "  H   a   INITIALDATA_TP%H_DW &   á"  H   a   INITIALDATA_TP%CNTRLV ,   )#  H   a   INITIALDATA_TP%CNTRLV_RATIO    q#  ±       GEOMETRY_TP &   "$     a   GEOMETRY_TP%REACHDISC (   ¶$     a   GEOMETRY_TP%REACHLENGTH &   J%     a   GEOMETRY_TP%REACHTYPE '   Þ%     a   GEOMETRY_TP%REACHSLOPE )   r&     a   GEOMETRY_TP%REACHMANNING '   '     a   GEOMETRY_TP%REACHWIDTH 