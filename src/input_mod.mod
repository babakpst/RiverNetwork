  �  8   k820309    �          17.0        oy�Z                                                                                                           
       Input_mod.f90 INPUT_MOD                                                     
                                                              u #INPUT_ADDRESS_SUB    #INPUT_ANALYSIS_SUB                      @               @                '�             	      #MODELNAME    #INPUTDIR    #ANALYSISDIR    #OUTPUTDIR    #ANALYSISOUTPUTDIR 	   #ANALYSESNAMES 
   #ANALYSISTYPE    #NUMBEROFANALYSES    #VERSION                 �                                                                      �                                   �                                  �                                   �       �                           �                                   �       J                          �                              	     �       �             .           �                              
            x      �                      &                                                                �                                   �                         �                                   �                         �                                   �      	   	                     @               @                '�                   #NOREACHES    #REACHDISC    #REACHTYPE    #REACHLENGTH    #REACHSLOPE    #REACHMANNING    #REACHWIDTH                 �                                                            �                                                                       &                                                      �                                          P                             &                                                      �                                          �                 
            &                                                      �                                          �                 
            &                                                      �                                          (                
            &                                                      �                                          p                
            &                                                             @                                '0                    #TOTALTIME    #TIMESTEP    #Q_UP    #H_DW    #CNTRLV    #CNTRLV_RATIO                 �                                              
                �                                             
                �                                             
                �                                             
                �                                              
                �                                   (          
                                                                                                                                                                                                                                                                                                X              600                                                                                    �              500                                            !     N                            O       �              C('ERROR IN ALLOCATING Arrays. ERROR NUMBER IS :', I4, '   LOCATION: ??????.')                                                             "     '                            (       �              C('OOPS!!!  FAIL TO OPERATE PROPERLY.')                                                             #                                        �              C('PRESS ENTER TO End ...')                                                             $     P                            Q       �              C( 'ERROR IN OPEN STATEMENT. Unit NUMBER=', I3, '   ERROR NUMBER IS=', I4  )                                                                 %     Q                            R       �              C( 'ERROR IN Close STATEMENT. Unit NUMBER=', I3, '   ERROR NUMBER IS=', I4  )                                                                 &                                       �              501                                            '     F                            G       �              C('ERROR IN READ STATEMENT. Unit IS : ',I5,' ERROR NUMBER IS : ', I5 )                                                             (     L                            M       �              C('End OF FILE IN READ STATEMENT. Unit IS : ',I5,' ERROR NUMBER IS : ', I5 )                                                             )     N                            O       �              C('End OF RECORD IN READ STATEMENT. Unit IS : ',I5,' ERROR NUMBER IS : ', I5 )                                                             *     G                            H       �              C('ERROR IN write STATEMENT. Unit IS : ',I5,' ERROR NUMBER IS : ', I5 )                                                             +                                       �              502                                            ,                                       �              510#         @      X                                                 #MODELINFO -             D                                 -     �              #INPUT_DATA_TP    #         @      X                                                 #I_ANALYSES .   #MODELINFO /   #ANALYSISINFO 0             
                                 .                     
D                                 /     �              #INPUT_DATA_TP              D P                                0     0               #ANALYSISDATA_TP    #         @                                   1                    #MODELINFO 2   #GEOMETRY 3             
                                  2     �             #INPUT_DATA_TP              D P                                3     �              #GEOMETRY_TP    #         @                                   4                    #MODELINFO 5   #GEOMETRY 6             
                                  5     �             #INPUT_DATA_TP              
D P                                6     �              #GEOMETRY_TP       �          fn#fn    �   @   J   PARAMETERS_MOD       o       gen@INPUT -   o  �       INPUT_DATA_TP+PARAMETERS_MOD 7   [  P   a   INPUT_DATA_TP%MODELNAME+PARAMETERS_MOD 6   �  P   a   INPUT_DATA_TP%INPUTDIR+PARAMETERS_MOD 9   �  P   a   INPUT_DATA_TP%ANALYSISDIR+PARAMETERS_MOD 7   K  P   a   INPUT_DATA_TP%OUTPUTDIR+PARAMETERS_MOD ?   �  P   a   INPUT_DATA_TP%ANALYSISOUTPUTDIR+PARAMETERS_MOD ;   �  �   a   INPUT_DATA_TP%ANALYSESNAMES+PARAMETERS_MOD :   �  H   a   INPUT_DATA_TP%ANALYSISTYPE+PARAMETERS_MOD >   �  H   a   INPUT_DATA_TP%NUMBEROFANALYSES+PARAMETERS_MOD 5     H   a   INPUT_DATA_TP%VERSION+PARAMETERS_MOD +   _  �       GEOMETRY_TP+PARAMETERS_MOD 5     H   a   GEOMETRY_TP%NOREACHES+PARAMETERS_MOD 5   g  �   a   GEOMETRY_TP%REACHDISC+PARAMETERS_MOD 5   �  �   a   GEOMETRY_TP%REACHTYPE+PARAMETERS_MOD 7   �  �   a   GEOMETRY_TP%REACHLENGTH+PARAMETERS_MOD 6   #  �   a   GEOMETRY_TP%REACHSLOPE+PARAMETERS_MOD 8   �  �   a   GEOMETRY_TP%REACHMANNING+PARAMETERS_MOD 6   K	  �   a   GEOMETRY_TP%REACHWIDTH+PARAMETERS_MOD /   �	  �       ANALYSISDATA_TP+PARAMETERS_MOD 9   ~
  H   a   ANALYSISDATA_TP%TOTALTIME+PARAMETERS_MOD 8   �
  H   a   ANALYSISDATA_TP%TIMESTEP+PARAMETERS_MOD 4     H   a   ANALYSISDATA_TP%Q_UP+PARAMETERS_MOD 4   V  H   a   ANALYSISDATA_TP%H_DW+PARAMETERS_MOD 6   �  H   a   ANALYSISDATA_TP%CNTRLV+PARAMETERS_MOD <   �  H   a   ANALYSISDATA_TP%CNTRLV_RATIO+PARAMETERS_MOD $   .  p       SMLL+PARAMETERS_MOD $   �  p       SHRT+PARAMETERS_MOD (     s       FILEINFO+PARAMETERS_MOD '   �  s       FILEADR+PARAMETERS_MOD )   �  �       FMT_ALLCT+PARAMETERS_MOD &   �  �       FMT_FL+PARAMETERS_MOD '   k  �       FMT_END+PARAMETERS_MOD -     �       FMT_ERR1_OPEN+PARAMETERS_MOD .   �  �       FMT_ERR1_CLOSE+PARAMETERS_MOD -   �  s       FILEDATAMODEL+PARAMETERS_MOD )     �       FMT_READ1+PARAMETERS_MOD )   �  �       FMT_READ2+PARAMETERS_MOD )   �  �       FMT_READ3+PARAMETERS_MOD *   �  �       FMT_WRITE1+PARAMETERS_MOD +   H  s       FILEDATAGEO+PARAMETERS_MOD )   �  s       UNINPTANA+PARAMETERS_MOD "   .  W       INPUT_ADDRESS_SUB ,   �  [   a   INPUT_ADDRESS_SUB%MODELINFO #   �  y       INPUT_ANALYSIS_SUB .   Y  @   a   INPUT_ANALYSIS_SUB%I_ANALYSES -   �  [   a   INPUT_ANALYSIS_SUB%MODELINFO 0   �  ]   a   INPUT_ANALYSIS_SUB%ANALYSISINFO     Q  e       INPUT_BASIC_SUB *   �  [   a   INPUT_BASIC_SUB%MODELINFO )     Y   a   INPUT_BASIC_SUB%GEOMETRY     j  e       INPUT_ARRAY_SUB *   �  [   a   INPUT_ARRAY_SUB%MODELINFO )   *  Y   a   INPUT_ARRAY_SUB%GEOMETRY 