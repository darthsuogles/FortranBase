	  Ö  P   k820309    ³
          11.1        b»IL                                                                                                       
       mGutenberg.f90 MGUTENBERG                                                
                                                          u #READREALMATRIX    #READCOMPLEXMATRIX                                                       u #WRITEREALMATRIX    #WRITECOMPLEXMATRIX                  @               @                '8            #IROWS    #ICOLS    #DATA 	                                                                                                                                                     	                 
        &           &                                     @               @           
     '8            #IROWS    #ICOLS    #DATA                                                                                                                                                                               &           &                                  @  @                               'l            #BASE    #LEN    #OFFSET    #FLAGS    #RANK    #RESERVED1    #DIMINFO                                                                                                                                                                                                                                                                                                                                                                                        #FOR_DESC_TRIPLET    p      p        p                         @  @                               '            #EXTENT    #MULT    #LOWERBOUND                                                                                                                                                                    #     @                                                  #REALMATRIXTYPE    #ALLOCREALMATRIX%PRESENT    #ALLOCREALMATRIX%ALLOCATED    #MATRIX    #IROWS    #ICOLS    #ERR_UNIT                                                  PRESENT                                              ALLOCATED       
                                      8       #REALMATRIXTYPE          
                                               
                                               
                                         #     @                                 !                 #COMPLEXMATRIXTYPE 
   #ALLOCCOMPLEXMATRIX%PRESENT "   #ALLOCCOMPLEXMATRIX%ALLOCATED #   #MATRIX $   #IROWS %   #ICOLS &   #ERR_UNIT '                                           "     PRESENT                                         #     ALLOCATED       
                                 $     8       #COMPLEXMATRIXTYPE 
         
                                  %             
                                  &             
                                 '       &     @                                (     8                 #REALMATRIXTYPE    #MATRIX )   #ALPHA *   #REALMATRIXTYPE          
                                  )     8      #REALMATRIXTYPE          
                                 *     
  &     @                                +     8                 #COMPLEXMATRIXTYPE 
   #MATRIX ,   #ALPHA -   #COMPLEXMATRIXTYPE 
         
                                  ,     8      #COMPLEXMATRIXTYPE 
         
                                 -     
  &     @                                .     8                 #REALMATRIXTYPE    #ALPHA /   #MATRIX 0   #REALMATRIXTYPE          
                                 /     
        
                                  0     8      #REALMATRIXTYPE    &     @                                1     8                 #COMPLEXMATRIXTYPE 
   #ALPHA 2   #MATRIX 3   #COMPLEXMATRIXTYPE 
         
                                 2     
        
                                  3     8      #COMPLEXMATRIXTYPE 
   &     @                                4     8                #REALMATRIXTYPE    #MULTREALMATRIX3%MATMUL 5   #MATRIX1 6   #MATRIX2 7   #REALMATRIXTYPE                                            5     MATMUL       
                                  6     8      #REALMATRIXTYPE          
                                  7     8      #REALMATRIXTYPE    &     @                                8     8                #COMPLEXMATRIXTYPE 
   #MULTCOMPLEXMATRIX3%MATMUL 9   #MATRIX1 :   #MATRIX2 ;   #COMPLEXMATRIXTYPE 
                                           9     MATMUL       
                                  :     8      #COMPLEXMATRIXTYPE 
         
                                  ;     8      #COMPLEXMATRIXTYPE 
   &     @                                <     8                 #REALMATRIXTYPE    #MATRIX1 =   #MATRIX2 >   #REALMATRIXTYPE          
                                  =     8      #REALMATRIXTYPE          
                                  >     8      #REALMATRIXTYPE    &     @                                ?     8                 #COMPLEXMATRIXTYPE 
   #MATRIX1 @   #MATRIX2 A   #COMPLEXMATRIXTYPE 
         
                                  @     8      #COMPLEXMATRIXTYPE 
         
                                  A     8      #COMPLEXMATRIXTYPE 
   #     @      X                                              #REALMATRIXTYPE    #MATRIX B   #FILE_NAME C         D @                               B     8       #REALMATRIXTYPE          
                                 C            1 #     @      X                                              #COMPLEXMATRIXTYPE 
   #MATRIX D   #FILE_NAME E         D @                               D     8       #COMPLEXMATRIXTYPE 
         
                                 E            1 #     @      X                                             #REALMATRIXTYPE    #WRITEREALMATRIX%PRESENT F   #MATRIX G   #UNIT H   #LABEL I                                          F     PRESENT       
                                  G     8      #REALMATRIXTYPE           @                               H                                              I             1 #     @      X                                             #COMPLEXMATRIXTYPE 
   #WRITECOMPLEXMATRIX%PRESENT J   #MATRIX K   #UNIT L   #LABEL M                                          J     PRESENT       
                                  K     8      #COMPLEXMATRIXTYPE 
          @                               L                                              M             1       "      fn#fn    ¾   <   J   MLINALG    ú   g       gen@READMATRIX     a  i       gen@WRITEMATRIX '   Ê  d       REALMATRIXTYPE+MLINALG -   .  @   a   REALMATRIXTYPE%IROWS+MLINALG -   n  @   a   REALMATRIXTYPE%ICOLS+MLINALG ,   ®  |   a   REALMATRIXTYPE%DATA+MLINALG *   *  d       COMPLEXMATRIXTYPE+MLINALG 0     @   a   COMPLEXMATRIXTYPE%IROWS+MLINALG 0   Î  @   a   COMPLEXMATRIXTYPE%ICOLS+MLINALG /     |   a   COMPLEXMATRIXTYPE%DATA+MLINALG 3           FOR_ARRAY_DESCRIPTOR+ISO_C_BINDING 8     @   a   FOR_ARRAY_DESCRIPTOR%BASE+ISO_C_BINDING 7   ^  @   a   FOR_ARRAY_DESCRIPTOR%LEN+ISO_C_BINDING :     @   a   FOR_ARRAY_DESCRIPTOR%OFFSET+ISO_C_BINDING 9   Þ  @   a   FOR_ARRAY_DESCRIPTOR%FLAGS+ISO_C_BINDING 8     @   a   FOR_ARRAY_DESCRIPTOR%RANK+ISO_C_BINDING =   ^  @   a   FOR_ARRAY_DESCRIPTOR%RESERVED1+ISO_C_BINDING ;        a   FOR_ARRAY_DESCRIPTOR%DIMINFO+ISO_C_BINDING /   ,  j      FOR_DESC_TRIPLET+ISO_C_BINDING 6     @   a   FOR_DESC_TRIPLET%EXTENT+ISO_C_BINDING 4   Ö  @   a   FOR_DESC_TRIPLET%MULT+ISO_C_BINDING :     @   a   FOR_DESC_TRIPLET%LOWERBOUND+ISO_C_BINDING (   V  Ä       ALLOCREALMATRIX+MLINALG 0   	  <      ALLOCREALMATRIX%PRESENT+MLINALG 2   V	  >      ALLOCREALMATRIX%ALLOCATED+MLINALG /   	  P   a   ALLOCREALMATRIX%MATRIX+MLINALG .   ä	  8   a   ALLOCREALMATRIX%IROWS+MLINALG .   
  8   a   ALLOCREALMATRIX%ICOLS+MLINALG 1   T
  8   a   ALLOCREALMATRIX%ERR_UNIT+MLINALG +   
  Í       ALLOCCOMPLEXMATRIX+MLINALG 3   Y  <      ALLOCCOMPLEXMATRIX%PRESENT+MLINALG 5     >      ALLOCCOMPLEXMATRIX%ALLOCATED+MLINALG 2   Ó  S   a   ALLOCCOMPLEXMATRIX%MATRIX+MLINALG 1   &  8   a   ALLOCCOMPLEXMATRIX%IROWS+MLINALG 1   ^  8   a   ALLOCCOMPLEXMATRIX%ICOLS+MLINALG 4     8   a   ALLOCCOMPLEXMATRIX%ERR_UNIT+MLINALG )   Î         MULTREALMATRIX1L+MLINALG 0   U  P   a   MULTREALMATRIX1L%MATRIX+MLINALG /   ¥  8   a   MULTREALMATRIX1L%ALPHA+MLINALG ,   Ý         MULTCOMPLEXMATRIX1L+MLINALG 3   j  S   a   MULTCOMPLEXMATRIX1L%MATRIX+MLINALG 2   ½  8   a   MULTCOMPLEXMATRIX1L%ALPHA+MLINALG )   õ         MULTREALMATRIX1R+MLINALG /   |  8   a   MULTREALMATRIX1R%ALPHA+MLINALG 0   ´  P   a   MULTREALMATRIX1R%MATRIX+MLINALG ,            MULTCOMPLEXMATRIX1R+MLINALG 2     8   a   MULTCOMPLEXMATRIX1R%ALPHA+MLINALG 3   É  S   a   MULTCOMPLEXMATRIX1R%MATRIX+MLINALG (     ¦       MULTREALMATRIX3+MLINALG /   Â  ;      MULTREALMATRIX3%MATMUL+MLINALG 0   ý  P   a   MULTREALMATRIX3%MATRIX1+MLINALG 0   M  P   a   MULTREALMATRIX3%MATRIX2+MLINALG +     ¯       MULTCOMPLEXMATRIX3+MLINALG 2   L  ;      MULTCOMPLEXMATRIX3%MATMUL+MLINALG 3     S   a   MULTCOMPLEXMATRIX3%MATRIX1+MLINALG 3   Ú  S   a   MULTCOMPLEXMATRIX3%MATRIX2+MLINALG &   -         ADDREALMATRIX+MLINALG .   ·  P   a   ADDREALMATRIX%MATRIX1+MLINALG .     P   a   ADDREALMATRIX%MATRIX2+MLINALG )   W         ADDCOMPLEXMATRIX+MLINALG 1   ç  S   a   ADDCOMPLEXMATRIX%MATRIX1+MLINALG 1   :  S   a   ADDCOMPLEXMATRIX%MATRIX2+MLINALG      s       READREALMATRIX &      P   a   READREALMATRIX%MATRIX )   P  @   a   READREALMATRIX%FILE_NAME "     v       READCOMPLEXMATRIX )     S   a   READCOMPLEXMATRIX%MATRIX ,   Y  @   a   READCOMPLEXMATRIX%FILE_NAME              WRITEREALMATRIX (   /  <      WRITEREALMATRIX%PRESENT '   k  P   a   WRITEREALMATRIX%MATRIX %   »  8   a   WRITEREALMATRIX%UNIT &   ó  @   a   WRITEREALMATRIX%LABEL #   3         WRITECOMPLEXMATRIX +   Ï  <      WRITECOMPLEXMATRIX%PRESENT *     S   a   WRITECOMPLEXMATRIX%MATRIX (   ^  8   a   WRITECOMPLEXMATRIX%UNIT )     @   a   WRITECOMPLEXMATRIX%LABEL 