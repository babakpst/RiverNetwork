  �  -   k820309              18.0        �o�Z                                                                                                          
       Input_mod.f90 INPUT_MOD                                                     
                                                              u #INPUT_ADDRESS_SUB    #INPUT_BASIC_SUB    #INPUT_ARRAY_SUB    #INPUT_ANALYSIS_SUB                      @               @                '�             	      #MODELNAME    #INPUTDIR    #ANALYSISDIR 	   #OUTPUTDIR 
   #ANALYSISOUTPUTDIR    #ANALYSESNAMES    #ANALYSISTYPE    #NUMBEROFANALYSES    #VERSION                 �                                                                      �                                   �                                  �                              	     �       �                           �                              
     �       J                          �                                   �       �             .           �                                          x      �                      &                                                                �                                   �                         �                                   �                         �                                   �      	   	                     @                                '8                    #NOREACHES    #TOTALTIME    #TIMESTEP    #Q_UP    #H_DW    #CNTRLV    #CNTRLV_RATIO                 �                                                              �                                             
                �                                             
                �                                             
                �                                              
                �                                   (          
                �                                   0          
                                                                                                                                                                                                                                                                                                X              600                                                                                   �              500                                                 N                            O       �              C('ERROR IN ALLOCATING Arrays. ERROR NUMBER IS :', I4, '   LOCATION: ??????.')                                                                  '                            (       �              C('OOPS!!!  FAIL TO OPERATE PROPERLY.')                                                                                                     �              C('PRESS ENTER TO End ...')                                                                  P                            Q       �              C( 'ERROR IN OPEN STATEMENT. Unit NUMBER=', I3, '   ERROR NUMBER IS=', I4  )                                                                       Q                            R       �              C( 'ERROR IN Close STATEMENT. Unit NUMBER=', I3, '   ERROR NUMBER IS=', I4  )                                                                 !                                       �              501                                            "     F                            G       �              C('ERROR IN READ STATEMENT. Unit IS : ',I5,' ERROR NUMBER IS : ', I5 )                                                             #     L                            M       �              C('End OF FILE IN READ STATEMENT. Unit IS : ',I5,' ERROR NUMBER IS : ', I5 )                                                             $     N                            O       �              C('End OF RECORD IN READ STATEMENT. Unit IS : ',I5,' ERROR NUMBER IS : ', I5 )                                                             %     G                            H       �              C('ERROR IN write STATEMENT. Unit IS : ',I5,' ERROR NUMBER IS : ', I5 )                                                             &                                       �              510#         @      X                                                 #MODELINFO '             D                                 '     �              #INPUT_DATA_TP    #         @      X                                                 #MODELINFO (   #INITIALINFO )             
                                  (     �             #INPUT_DATA_TP              D P                                )     8               #INITIALDATA_TP    #         @      X                                                  #         @      X                                                 #I_ANALYSIS *   #MODELINFO +             
                                 *                     
D                                 +     �              #INPUT_DATA_TP       �          fn#fn    �   @   J   PARAMETERS_MOD       �       gen@INPUT -   �  �       INPUT_DATA_TP+PARAMETERS_MOD 7   �  P   a   INPUT_DATA_TP%MODELNAME+PARAMETERS_MOD 6   �  P   a   INPUT_DATA_TP%INPUTDIR+PARAMETERS_MOD 9   %  P   a   INPUT_DATA_TP%ANALYSISDIR+PARAMETERS_MOD 7   u  P   a   INPUT_DATA_TP%OUTPUTDIR+PARAMETERS_MOD ?   �  P   a   INPUT_DATA_TP%ANALYSISOUTPUTDIR+PARAMETERS_MOD ;     �   a   INPUT_DATA_TP%ANALYSESNAMES+PARAMETERS_MOD :   �  H   a   INPUT_DATA_TP%ANALYSISTYPE+PARAMETERS_MOD >   �  H   a   INPUT_DATA_TP%NUMBEROFANALYSES+PARAMETERS_MOD 5   A  H   a   INPUT_DATA_TP%VERSION+PARAMETERS_MOD .   �  �       INITIALDATA_TP+PARAMETERS_MOD 8   7  H   a   INITIALDATA_TP%NOREACHES+PARAMETERS_MOD 8     H   a   INITIALDATA_TP%TOTALTIME+PARAMETERS_MOD 7   �  H   a   INITIALDATA_TP%TIMESTEP+PARAMETERS_MOD 3     H   a   INITIALDATA_TP%Q_UP+PARAMETERS_MOD 3   W  H   a   INITIALDATA_TP%H_DW+PARAMETERS_MOD 5   �  H   a   INITIALDATA_TP%CNTRLV+PARAMETERS_MOD ;   �  H   a   INITIALDATA_TP%CNTRLV_RATIO+PARAMETERS_MOD $   /  p       SMLL+PARAMETERS_MOD $   �  p       SHRT+PARAMETERS_MOD (   	  s       FILEINFO+PARAMETERS_MOD '   �	  s       FILEADR+PARAMETERS_MOD )   �	  �       FMT_ALLCT+PARAMETERS_MOD &   �
  �       FMT_FL+PARAMETERS_MOD '   l  �       FMT_END+PARAMETERS_MOD -     �       FMT_ERR1_OPEN+PARAMETERS_MOD .   �  �       FMT_ERR1_CLOSE+PARAMETERS_MOD -   �  s       FILEDATAMODEL+PARAMETERS_MOD )     �       FMT_READ1+PARAMETERS_MOD )   �  �       FMT_READ2+PARAMETERS_MOD )   �  �       FMT_READ3+PARAMETERS_MOD *   �  �       FMT_WRITE1+PARAMETERS_MOD )   I  s       UNINPTANA+PARAMETERS_MOD "   �  W       INPUT_ADDRESS_SUB ,     [   a   INPUT_ADDRESS_SUB%MODELINFO     n  h       INPUT_BASIC_SUB *   �  [   a   INPUT_BASIC_SUB%MODELINFO ,   1  \   a   INPUT_BASIC_SUB%INITIALINFO     �  H       INPUT_ARRAY_SUB #   �  g       INPUT_ANALYSIS_SUB .   <  @   a   INPUT_ANALYSIS_SUB%I_ANALYSIS -   |  [   a   INPUT_ANALYSIS_SUB%MODELINFO 